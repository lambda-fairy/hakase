{-# LANGUAGE DeriveFoldable, OverloadedStrings #-}

module Hakase.Server where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Attoparsec.Combinator (endOfInput)
import Data.Foldable
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Data.Word (Word32)
import Network.Simple.TCP hiding (recv, send)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams
import System.Timeout (timeout)

import Hakase.Common

import qualified Paths_hakase_server


serverVersion :: Text
serverVersion = Text.pack $ "Hakase/" ++ showVersion Paths_hakase_server.version


hakaseServer :: HostPreference -> ServiceName -> IO r
hakaseServer hp port = do
    lobbyChan <- newChan
    bracket (forkIO $ matchmaker lobbyChan) killThread $ \_ ->
        listen hp port $ \(lsock, _) ->
            forever $ acceptFork lsock (handshake lobbyChan)


handshake :: Chan (Client Ready, MVar Invite) -> (Socket, SockAddr) -> IO ()
handshake lobbyChan (sock, addr) = do
    putStrLn $ "client connected: " ++ show addr
    (is, os) <- Streams.socketToStreams sock
    is' <- Streams.parserToInputStream parseCommand' is
    let client = Client
            { maybeRecv = Streams.read is'
            , send = \c -> Streams.write (Just $ renderCommand c) os
            , clientMoves = Handshaking
            , clientName = Handshaking
            }
    -- Perform the handshake
    client' <- handshake' client
    -- Wait for a challenger to appear
    inviteVar <- newEmptyMVar
    writeChan lobbyChan (client', inviteVar)
    invite <- takeMVar inviteVar
    -- Play the game!
    loop invite client'
        -- Make sure that the channel is closed afterward
        `finally` writeChan (ready $ clientMoves client') Nothing
  where
    parseCommand' = Nothing <$ endOfInput <|> Just <$> parseCommand


matchmaker :: Chan (Client Ready, MVar Invite) -> IO a
matchmaker lobbyChan = forever $ do
    (whiteClient, whiteVar) <- readChan lobbyChan
    (blackClient, blackVar) <- readChan lobbyChan
    putMVar whiteVar Invite
        { numberOfMoves = defaultNumberOfMoves
        , opponentMoves = ready $ clientMoves blackClient
        , opponentName = ready $ clientName blackClient
        }
    putMVar blackVar Invite
        { numberOfMoves = defaultNumberOfMoves
        , opponentMoves = ready $ clientMoves whiteClient
        , opponentName = ready $ clientName whiteClient
        }
  where
    defaultNumberOfMoves = 10  -- lol


data Client state = Client
    { maybeRecv :: !(IO (Maybe Command))
    , send :: !(Command -> IO ())
    , clientName :: !(state Text)
    , clientMoves :: !(state (Chan (Maybe Move)))
    }

data Handshaking a = Handshaking
    deriving Foldable

newtype Ready a = Ready { ready :: a }
    deriving Foldable

recv :: Foldable state => Client state -> IO Command
recv client = do
    m <- timeout (5 * 1000 * 1000) $ maybeRecv client  -- 5 seconds
    case m of
        Just (Just c) -> return c
        Just Nothing -> kick client "connection lost"
        Nothing -> kick client "too slow"


data ClientKicked = ClientKicked
    { clientKickedName :: !(Maybe Text)
    , clientKickedReason :: !Text
    }
    deriving Show

instance Exception ClientKicked

kick :: Foldable state => Client state -> Text -> IO a
kick client reason = do
    send client $ Kick reason
    throw $ ClientKicked (toMaybe $ clientName client) reason
  where
    toMaybe = getFirst . foldMap (First . Just)


handshake' :: Client Handshaking -> IO (Client Ready)
handshake' client =
    recv client >>= \c -> case c of
        Hello name version | version == protocolVersion -> do
            send client $ Welcome serverVersion
            moves <- newChan
            return client
                { clientName = Ready name
                , clientMoves = Ready moves
                }
        Hello _ version ->
            kick client $ "unsupported client version: " <> textShow version
        _ ->
            kick client $ "unexpected command: " <> textShow c


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
        recv client >>= \c -> case c of
            Move move -> writeChan (ready $ clientMoves client) $ Just move
            _ -> kick client $ "unexpected command: " <> textShow c
        -- Send them their opponent's latest move
        opponentMove <- readChan (opponentMoves invite)
        case opponentMove of
            Just move -> send client $ Move move
            Nothing -> kick client "opponent disconnected :("
