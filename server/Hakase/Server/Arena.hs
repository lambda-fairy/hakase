{-# LANGUAGE DeriveFoldable, OverloadedStrings #-}

module Hakase.Server.Arena
    ( hakaseServer
    , ServerConfig(..)
    , socketToHakaseStreams
    , makeHakaseStreams
    , Ticket()
    , ticketClientName
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Attoparsec.Combinator (endOfInput)
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Data.Void (Void, vacuous)
import Network.Socket (Socket)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams
import System.Timeout (timeout)

import Hakase.Server.Common


-- | Run the Hakase server.
--
-- To allow for easier mocking, @hakaseServer@ doesn't listen to the network on
-- its own. Instead, the user must provide a 'configListenClients' callback that
-- listens for clients and hands them off to the main loop. A typical
-- implementation may look like this:
--
-- @
-- hakaseServer 'ServerConfig'
--     { 'configListenClients' = \\continue ->
--         'forever' $ do
--             -- Accept an incoming connection
--             sock <- ...
--             'forkIO' $ do
--                 -- Wrap the socket in a pair of message streams
--                 st <- 'socketToHakaseStreams' sock
--                 -- Hand the client off to the main loop
--                 continue st
--     -- ...
--     }
-- @
hakaseServer :: ServerConfig r -> IO r
hakaseServer c = do
    lobbyChan <- newChan  -- moe desu
    bracket (forkIO . vacuous $ matchmaker c lobbyChan) killThread $ \_ ->
        configListenClients c (accept c lobbyChan)


data ServerConfig r = ServerConfig
    { configListenClients :: ((InputStream Message, OutputStream Message) -> IO ()) -> IO r
    , configCheckPlayer :: Text -> Text -> IO Bool
    , configMatchPlayers :: Chan Ticket -> (Ticket -> Ticket -> IO ()) -> IO Void
    , configRecordBattle :: Battle -> IO ()
    , configNumberOfMoves :: Word32
    }


-- | Construct a pair of message streams from a 'Socket'.
socketToHakaseStreams
    :: Socket -> IO (InputStream Message, OutputStream Message)
socketToHakaseStreams = Streams.socketToStreams >=> makeHakaseStreams


-- | Given a pair of raw byte streams, construct a pair of streams on parsed
-- 'Message's.
makeHakaseStreams
    :: (InputStream ByteString, OutputStream ByteString)
    -> IO (InputStream Message, OutputStream Message)
makeHakaseStreams (is, os) =
    (,) <$> Streams.parserToInputStream parseMessage' is
        <*> Streams.contramap renderMessage os
  where
    parseMessage' = Nothing <$ endOfInput <|> Just <$> parseMessage


-- | Represents a client which has registered successfully with the server, but
-- has not yet been assigned to a game.
data Ticket = Ticket
    { ticketClient :: !(Client Ready)
        -- ^ The client who is waiting for a game.
    , _ticketInvite :: !(MVar Invite)
        -- ^ When this variable is filled, the game will begin.
    }

-- | Get the name of the client for this ticket.
ticketClientName :: Ticket -> Text
ticketClientName = ready . clientName . ticketClient


accept
    :: ServerConfig r
    -> Chan Ticket
    -> (InputStream Message, OutputStream Message)
    -> IO ()
accept c lobbyChan (is, os) = do
    let client = Client
            { maybeRecv = Streams.read is
            , send = \message -> Streams.write (Just message) os
            , clientMoves = Handshaking
            , clientName = Handshaking
            }
    -- Perform the handshake
    client' <- handshake c client
    -- Wait for a challenger to appear
    inviteVar <- newEmptyMVar
    writeChan lobbyChan $ Ticket client' inviteVar
    invite <- takeMVar inviteVar
    -- Play the game!
    loop invite client'
        -- Make sure that the channel is closed afterward
        `finally` writeChan (ready $ clientMoves client') Nothing


matchmaker :: ServerConfig r -> Chan Ticket -> IO Void
matchmaker c lobbyChan = configMatchPlayers c lobbyChan $ \white black -> do
    let Ticket whiteClient whiteVar = white
    let Ticket blackClient blackVar = black
    -- Duplicate the event streams beforehand, so that we can record what's
    -- going on (see below)
    whiteMoves <- dupChan . ready $ clientMoves whiteClient
    blackMoves <- dupChan . ready $ clientMoves blackClient
    -- Start the game!
    putMVar whiteVar Invite
        { numberOfMoves = configNumberOfMoves c
        , opponentMoves = ready $ clientMoves blackClient
        , opponentName = ready $ clientName blackClient
        }
    putMVar blackVar Invite
        { numberOfMoves = configNumberOfMoves c
        , opponentMoves = ready $ clientMoves whiteClient
        , opponentName = ready $ clientName whiteClient
        }
    -- Record the result of this epic battle!
    void . forkIO $ do
        whiteMoves' <- takeWhileJust <$> getChanContents whiteMoves
        blackMoves' <- takeWhileJust <$> getChanContents blackMoves
        -- Since getChanContents is lazy, it won't read the channel until we
        -- force the resulting list
        _ <- evaluate $ length whiteMoves'
        _ <- evaluate $ length blackMoves'
        configRecordBattle c Battle
            { battleWhite = ready $ clientName whiteClient
            , battleBlack = ready $ clientName blackClient
            , battleWhiteMoves = whiteMoves'
            , battleBlackMoves = blackMoves'
            }
  where
    takeWhileJust :: [Maybe a] -> [a]
    takeWhileJust = map fromJust . takeWhile isJust


-- | Stores information relating to a particular client.
--
-- The @state@ parameter represents how much we know about the client. It can be
-- either 'Handshaking' or 'Ready'.
data Client state = Client
    { maybeRecv :: !(IO (Maybe Message))
    , send :: !(Message -> IO ())
    , clientName :: !(state Text)
    , clientMoves :: !(state (Chan (Maybe Move)))
    }

-- | The client is still handshaking. There are still things we don't know.
data Handshaking a = Handshaking
    deriving Foldable

-- | The client is ready for action. All information is laid bare.
newtype Ready a = Ready { ready :: a }
    deriving Foldable

recv :: Foldable state => Client state -> IO Message
recv client = do
    m <- timeout (5 * 1000 * 1000) $ maybeRecv client  -- 5 seconds
    case m of
        Just (Just message) -> return message
        Just Nothing -> kick client "connection lost"
        Nothing -> kick client "too slow"


data ClientKicked = ClientKicked
    { _clientKickedName :: !(Maybe Text)
    , _clientKickedReason :: !Text
    }
    deriving Show

instance Exception ClientKicked

kick :: Foldable state => Client state -> Text -> IO a
kick client reason = do
    send client $ Kick reason
    throw $ ClientKicked (toMaybe $ clientName client) reason
  where
    toMaybe = getFirst . foldMap (First . Just)


handshake :: ServerConfig r -> Client Handshaking -> IO (Client Ready)
handshake c client =
    recv client >>= \message -> case message of
        Hello version name secret | version == protocolVersion -> do
            ok <- configCheckPlayer c name secret
            when (not ok) $ kick client "authentication failed"
            send client $ Welcome (Text.pack hakaseVersion)
            moves <- newChan
            return client
                { clientName = Ready name
                , clientMoves = Ready moves
                }
        Hello version _ _ ->
            kick client $ "unsupported client version: " <> textShow version
        _ ->
            kick client $ "unexpected message: " <> textShow message


data Invite = Invite
    { numberOfMoves :: !Word32
    , opponentMoves :: !(Chan (Maybe Move))
    , opponentName :: !Text
    }

loop :: Invite -> Client Ready -> IO ()
loop invite client = do
    -- Start the game
    send client $ Start (opponentName invite) (numberOfMoves invite)
    for_ [1 .. numberOfMoves invite] $ \_ -> do
        -- Receive this player's next move
        recv client >>= \message -> case message of
            Move move -> writeChan (ready $ clientMoves client) $ Just move
            _ -> kick client $ "unexpected message: " <> textShow message
        -- Send them their opponent's latest move
        opponentMove <- readChan (opponentMoves invite)
        case opponentMove of
            Just move -> send client $ Move move
            Nothing -> kick client "opponent disconnected :("
