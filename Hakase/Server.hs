{-# LANGUAGE DeriveFoldable, OverloadedStrings #-}

module Hakase.Server where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Foldable
import Data.Monoid
import Data.Text (Text)
import Data.Traversable
import Network.Simple.TCP hiding (recv, send)
import qualified System.IO.Streams as Streams

import Hakase.Common


hakaseServer :: HostPreference -> ServiceName -> IO r
hakaseServer hp port = do
    clientChan <- newChan
    bracket (forkIO $ matchmaker clientChan) killThread $ \_ ->
        listen hp port $ \(lsock, _) ->
            forever $ acceptFork lsock (handshake clientChan)


handshake :: Chan (Client HasName) -> (Socket, SockAddr) -> IO ()
handshake clientChan (sock, addr) = do
    putStrLn $ "client connected: " ++ show addr
    closeVar <- newEmptyMVar
    (is, os) <- Streams.socketToStreams sock
    is' <- Streams.lines is
    let client = Client
            { maybeRecv = do
                maybeLine <- Streams.read is'
                for maybeLine $ \line ->
                    case decodeStrict line of
                        Just c -> return c
                        Nothing -> kick client "could not parse command"
            , send = \c -> Streams.writeLazyByteString (encode c) os
            , close = void $ tryPutMVar closeVar ()
            , nameF = NoName
            }
    client' <- handshake' client
    writeChan clientChan client'
    takeMVar closeVar


matchmaker :: Chan (Client HasName) -> IO a
matchmaker = error "unimplemented"


data Client f = Client
    { maybeRecv :: IO (Maybe Command)
    , send :: Command -> IO ()
    , close :: IO ()
    , nameF :: f Text
    }

newtype HasName a = HasName a
    deriving Foldable

data NoName a = NoName
    deriving Foldable

recv :: Foldable f => Client f -> IO Command
recv client = do
    m <- maybeRecv client
    case m of
        Just c -> return c
        Nothing -> kick client "connection lost"


data ClientKicked = ClientKicked { clientKickedName :: !(Maybe Text) }
    deriving Show

instance Exception ClientKicked

kick :: Foldable f => Client f -> Text -> IO a
kick client reason = do
    send client $ Kick reason
    close client
    throw $ ClientKicked (toMaybe $ nameF client)
  where
    toMaybe = getFirst . foldMap (First . Just)


handshake' :: Client NoName -> IO (Client HasName)
handshake' client =
    recv client >>= \c -> case c of
        Hello name version | version == 0 -> do
            send client $ Welcome "Hakase!!!"
            return client { nameF = HasName name }
        Hello _ version ->
            kick client $ "unsupported client version: " <> textShow version
        _ ->
            kick client $ "unexpected command: " <> textShow c


loop :: [(MVar Move, MVar Move)] -> Text -> Client HasName -> IO ()
loop moves otherName client = do
    -- Start the game
    send client $ Start otherName (length moves)
    for_ moves $ \(myMoveRef, otherMoveRef) -> do
        -- Receive this player's next move
        recv client >>= \c -> case c of
            Move myMove -> putMVar myMoveRef myMove  -- OK!
            _ -> kick client $ "unexpected command: " <> textShow c
        -- Send them their opponent's latest move
        otherMove <- readMVar otherMoveRef
        send client $ Move otherMove
