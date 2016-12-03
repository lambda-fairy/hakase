{-# LANGUAGE OverloadedStrings #-}

module Hakase.Client where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (decodeStrict, encode)
import Data.Monoid ((<>))
import Network.Simple.TCP (HostName, ServiceName)
import qualified Network.Simple.TCP as Network
import System.IO.Error (eofErrorType, ioError, mkIOError, userError)
import qualified System.IO.Streams as Streams

import Hakase.Common


data Hakase = Hakase
    { recv :: IO Command
    , send :: Command -> IO ()
    }


connect
    :: (MonadIO m, MonadMask m)
    => HostName -> ServiceName
    -> (Hakase -> m r) -> m r
connect h p k = Network.connect h p $ \(sock, _) -> do
    (is, os) <- liftIO $ Streams.socketToStreams sock
    is' <- liftIO $ Streams.lines is
    k Hakase
        { recv = do
            -- Maybe she's born with it
            -- Maybe it's
            maybeLine <- Streams.read is'
            case maybeLine of
                Just line -> case decodeStrict line of
                    Just c -> return c
                    Nothing -> ioError invalidError
                Nothing -> ioError eofError
        , send = \c -> Streams.writeLazyByteString (encode c <> "\n") os
        }
  where
    invalidError = userError "could not parse command from server"
    eofError = mkIOError eofErrorType "end of stream" Nothing Nothing
