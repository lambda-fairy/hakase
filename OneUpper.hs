{-# LANGUAGE OverloadedStrings #-}

module OneUpper where

import Hakase.Client


main :: IO ()
main = hakase (\_ _ -> (Rock, ())) (\lastMove () -> (dominator lastMove, ()))
    "OneUpper" "localhost" "31337"
  where
    dominator m = toEnum ((fromEnum m + 1) `mod` 3)
