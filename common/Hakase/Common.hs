{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Hakase.Common
    ( -- * Core types
      protocolVersion
    , Command(..)
    , Move(..)
    , winner

      -- * Serialization
    , parseCommand
    , renderCommand

      -- * Miscellany
    , textShow
    ) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Generics (Generic)

import Hakase.Parse
import Hakase.Render


-- | The current wire protocol version.
protocolVersion :: Word32
protocolVersion = 0


-- | Represents a message in the Hakase protocol.
data Command
    = Hello { hello_clientName :: !Text, hello_clientVersion :: !Word32 }
        -- ^ Selamat siang. The first message sent by the client.
    | Welcome { welcome_serverName :: !Text }
        -- ^ Selamat datang. The first message sent by the server.
    | Start { start_opponentName :: !Text, start_numberOfMoves :: !Word32 }
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

instance Parse Command
instance Render Command


-- | Represents a move in the great game of Rock Paper Scissors.
data Move = Rock | Paper | Scissors
    deriving (Eq, Enum, Show, Generic)

instance Parse Move
instance Render Move


-- | Given the moves of two opposing players, decide who wins the round.
--
-- Returns @LT@ if the first player wins, @GT@ if the second player wins, or
-- @EQ@ if it's a draw.
winner :: Move -> Move -> Ordering
winner a b = [EQ, LT, GT] !! mod (fromEnum a - fromEnum b) 3
    -- e.g. Rock loses against Paper, but wins against Scissors


-- | Parse a single 'Command', including its trailing CRLF.
--
-- This is an inverse of 'renderCommand'.
parseCommand :: Parser Command
parseCommand = parse <* "\r\n"


-- | Render a 'Command' to a 'ByteString', appending a trailing CRLF.
--
-- This is an inverse of 'parseCommand'.
renderCommand :: Command -> ByteString
renderCommand c = render c <> "\r\n"


-- | Like 'show', but converts to a 'Text' instead of a 'String'.
textShow :: Show a => a -> Text
textShow = Text.pack . show
