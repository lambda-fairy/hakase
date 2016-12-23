{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Data.Acid
import Data.Monoid
import Network.Simple.TCP
import Options.Applicative

import Hakase.Server


main :: IO ()
main = do
    (hp, port) <- execParser opts
    bracket
        (openLocalState defaultServerState)
        closeAcidState
        (\acid -> hakaseServer ServerConfig
            { configListenClients = \continue ->
                listen hp port $ \(lsock, laddr) -> do
                    putStrLn $ "starting server on " ++ show laddr
                    forever $ acceptFork lsock $ \(sock, addr) -> do
                        putStrLn $ "client connected: " ++ show addr
                        st <- socketToHakaseStreams sock
                        continue st
            , configCheckPlayer = \name secret ->
                query acid $ CheckPlayer name secret
            , configMatchPlayers = \lobbyChan invite -> forever $ do
                -- Pick the first two clients who arrive
                -- FIXME(#5): do something more clever than this
                white <- readChan lobbyChan
                black <- readChan lobbyChan
                invite white black
            , configRecordBattle = update acid . RecordBattle
            , configNumberOfMoves = 10
            })
  where
    opts = info (helper <*> args)
        ( fullDesc
        <> progDesc "Run a server that speaks the Hakase Rock-Paper-Scissors protocol"
        <> header "hakase-server - a server that speaks the Hakase Rock-Paper-Scissors protocol" )

    args = (,)
        <$> argument auto
            ( metavar "HOST"
            <> value "localhost"
            <> showDefault
            <> help "Host name to bind to" )
        <*> strArgument
            ( metavar "PORT"
            <> value "6266"
            <> showDefault
            <> help "Port to bind to" )
