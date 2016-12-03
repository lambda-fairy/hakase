{-# LANGUAGE DeriveGeneric #-}

module Hakase.Common where

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics


-- | Represents a message in the Hakase protocol.
data Command
    = Hello { hello_clientName :: !Text, hello_clientVersion :: !Int }
        -- ^ Selamat siang. The first message sent by the client.
    | Welcome { welcome_serverName :: !Text }
        -- ^ Selamat datang. The first message sent by the server.
    | Start { start_opponentName :: !Text, start_numberOfMoves :: !Int }
        -- ^ Starts a new round. This is sent from the server to the client.
    | Move { move_move :: !Move }
        -- ^ Represents a move in the game.
        --
        -- If sent from the client to the server, this contains the client's
        -- next move.
        --
        -- If sent from the server to the client, this contains the opponent's
        -- latest move.
    | Kick { kick_reason :: !Text }
        -- ^ Fatal error. This is sent from the server to the client, right
        -- before the server cuts the connection. The argument is a
        -- (human-readable) message describing the error.
    deriving (Eq, Show, Generic)

instance FromJSON Command where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Command where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions


-- | Represents a move in the great game of Rock Paper Scissors.
data Move = Rock | Paper | Scissors
    deriving (Eq, Enum, Show, Generic)

instance FromJSON Move where
    parseJSON = genericParseJSON aesonOptions { allNullaryToStringTag = True }

instance ToJSON Move where
    toJSON = genericToJSON aesonOptions { allNullaryToStringTag = True }
    toEncoding = genericToEncoding aesonOptions { allNullaryToStringTag = True }


aesonOptions :: Options
aesonOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '_' . tail . dropWhile (/= '_')
    , constructorTagModifier = map toUpper
    , allNullaryToStringTag = False
    }


-- | Given the moves of two opposing players, decide who wins the round.
--
-- Returns @LT@ if the first player wins, @GT@ if the second player wins, or
-- @EQ@ if it's a draw.
winner :: Move -> Move -> Ordering
winner a b = [EQ, LT, GT] !! mod (fromEnum a - fromEnum b) 3
    -- e.g. Rock loses against Paper, but wins against Scissors


textShow :: Show a => a -> Text
textShow = Text.pack . show
