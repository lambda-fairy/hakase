{-# LANGUAGE OverloadedStrings #-}

module Hakase.Client where

import Control.Applicative ((<|>))
import Control.Exception (throw)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Attoparsec.Combinator (endOfInput)
import Network.Simple.TCP (HostName, ServiceName)
import qualified Network.Simple.TCP as Network
import System.IO.Error (eofErrorType, mkIOError)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

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
    is' <- liftIO $ Streams.parserToInputStream parseCommand' is
    k Hakase
        { recv = Streams.read is' >>= maybe (throw eofError) return
        , send = \c -> Streams.write (Just $ renderCommand c) os
        }
  where
    parseCommand' = Nothing <$ endOfInput <|> Just <$> parseCommand
    eofError = mkIOError eofErrorType "end of stream" Nothing Nothing
