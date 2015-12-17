{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Lemon.Protocol.Types where


import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Version
import Data.Word
import GHC.Generics

import qualified Paths_lemon


protocolVersion :: Word32
protocolVersion = 0


serverVersion :: Text
serverVersion = Text.pack $ "Lemon/" ++ showVersion Paths_lemon.version


data Message
    = Ayy !Word32 !Text !ByteString
    | Lmao !Word32 !Text
    | Start !Word32 !Text !Text
    | Move !Player !Move
    | Done
    | Error !Error
    deriving (Show, Generic)


data Player = White | Black
    deriving (Eq, Ord, Bounded, Enum, Show)


other :: Player -> Player
other White = Black
other Black = White


data Move = Rock | Paper | Scissors
    deriving Show


data Error = KnownError !KnownError | OtherError !Text
    deriving Show


data KnownError
    = MismatchedVersion
    | InvalidCommand
    | NameAlreadyTaken
    | IncorrectSecret
    deriving (Read, Show)
