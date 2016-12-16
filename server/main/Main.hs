{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
            , configCheckPlayer = query acid . CheckPlayer
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
