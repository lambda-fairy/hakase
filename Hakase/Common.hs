{-# LANGUAGE DeriveGeneric #-}

module Hakase.Common where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics


-- | Represents a message in the Hakase protocol.
data Command
    = Hello !Text !Int
        -- ^ Selamat siang. The first message sent by the client.
        --
        -- The two arguments are the client's name and version, respectively.
    | Welcome !Text
        -- ^ Selamat datang. The first message sent by the server.
        --
        -- The argument is the server name.
    | Start !Text !Int
        -- ^ Starts a new round. This is sent from the server to the client.
        --
        -- The two arguments are the opponent's name and number of rounds,
        -- respectively.
    | Move !Move
        -- ^ Represents a move in the game.
        --
        -- If sent from the client to the server, this contains the client's
        -- next move.
        --
        -- If sent from the server to the client, this contains the opponent's
        -- latest move.
    | Kick !Text
        -- ^ Fatal error. This is sent from the server to the client, right
        -- before the server cuts the connection. The argument is a
        -- (human-readable) message describing the error.
    deriving (Eq, Show, Generic)

instance FromJSON Command

instance ToJSON Command where
    toEncoding = genericToEncoding defaultOptions


-- | Represents a move in the great game of Rock Paper Scissors.
data Move = Rock | Paper | Scissors
    deriving (Eq, Enum, Show, Generic)

instance FromJSON Move

instance ToJSON Move where
    toEncoding = genericToEncoding defaultOptions

-- | Given the moves of two opposing players, decide who wins the round.
--
-- Returns @LT@ if the first player wins, @GT@ if the second player wins, or
-- @EQ@ if it's a draw.
winner :: Move -> Move -> Ordering
winner a b = [EQ, LT, GT] !! mod (fromEnum a - fromEnum b) 3
    -- e.g. Rock loses against Paper, but wins against Scissors


textShow :: Show a => a -> Text
textShow = Text.pack . show
