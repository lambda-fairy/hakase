module Hakase.Server.Common
    ( Battle(..)
    , hakaseVersion
    , module Hakase.Common
    ) where

import Data.Text (Text)
import Data.Version (showVersion)

import Hakase.Common

import qualified Paths_hakase_server


data Battle = Battle
    { battleWhite :: !Text
    , battleBlack :: !Text
    , battleWhiteMoves :: ![Move]
    , battleBlackMoves :: ![Move]
    } deriving Show


-- | The server version.
hakaseVersion :: String
hakaseVersion = "Hakase/" ++ showVersion Paths_hakase_server.version
