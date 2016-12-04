{-# LANGUAGE OverloadedStrings #-}

module SimpleServer where

import Hakase.Server


main :: IO ()
main = do
    putStrLn "Here goes nothing!"
    hakaseServer "localhost" "31337"
