{-# LANGUAGE OverloadedStrings #-}

module SimpleClient where

import qualified Data.Text.IO as Text
import Data.Foldable
import Data.Monoid

import Hakase.Client
import Hakase.Common


main :: IO ()
main = connect "localhost" "31337" $ \h -> do
    send h $ Hello "RockStar" 0
    Welcome server <- recv h
    Text.putStrLn $ "connected to " <> server
    Start opponent numMoves <- recv h
    Text.putStrLn $ "matched with opponent " <> opponent <> ", "
        <> textShow numMoves <> " rounds"
    for_ [1 .. numMoves] $ \_ -> do
        send h $ Move Rock
        recv h >>= print
