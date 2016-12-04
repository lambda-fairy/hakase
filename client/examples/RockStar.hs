{-# LANGUAGE OverloadedStrings #-}

-- | Always throws Rock.

import Hakase.Client

main :: IO ()
main = hakase (\_ _ -> (Rock, ())) (\_ _ -> (Rock, ())) "RockStar"
    "localhost" "31337"
