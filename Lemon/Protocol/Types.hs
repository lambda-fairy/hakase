{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Lemon.Protocol.Types where


import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word
import GHC.Generics


data Message
    = Ayy !Word32 !Text !ByteString
    | Lmao !Word32 !Text
    | Come !Word32
    | Move !Move
    | Reply !Move
    | Done
    | Error !Error
    deriving (Show, Generic)


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
