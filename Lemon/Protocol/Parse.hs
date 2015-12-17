{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}

module Lemon.Protocol.Parse where


import Control.Applicative
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics
import Text.Read

import Lemon.Protocol.Types


messageParser :: Parser (Maybe Message)
messageParser = many nl *>
    (Just <$> rawMessage <* nl <|> Nothing <$ P.endOfInput)
  where
    rawMessage = to <$> gparse
    nl = "\n" <|> "\r\n"


class Parse a where
    parse :: Parser a

instance Parse Word32 where
    parse = P.decimal

instance Parse Text where
    parse = parse >>= either (fail . show) return . T.decodeUtf8'

instance Parse ByteString where
    parse = P.scan (0 :: Int) $ \n c ->
        if n < 1024 && c /= ':' && c /= '\r' && c /= '\n'
            then Just $! n + 1
            else Nothing

instance Parse Player where
    parse = White <$ "0" <|> Black <$ "1"

instance Parse Move where
    parse = Rock <$ "0" <|> Paper <$ "1" <|> Scissors <$ "2"

instance Parse Error where
    parse = convert <$> parse
      where
        convert s = maybe (OtherError s) KnownError $ readMaybe (T.unpack s)


class GParse f where
    gparse :: Parser (f p)

instance GParse V1 where
    gparse = empty

instance GParse U1 where
    gparse = pure U1

instance (GParse f, GParse g) => GParse (f :+: g) where
    gparse = L1 <$> gparse <|> R1 <$> gparse

instance (GParse f, GParse g) => GParse (f :*: g) where
    gparse = (:*:) <$> gparse <*> gparse

instance Parse c => GParse (K1 i c) where
    gparse = K1 <$ ":" <*> parse

instance forall c f. (GParse f, Constructor c) => GParse (C1 c f) where
    gparse = M1 <$ P.stringCI name <*> gparse
      where
        name = BC.pack $ conName (undefined :: C1 c f ())

instance GParse f => GParse (M1 D c f) where
    gparse = M1 <$> gparse

instance GParse f => GParse (M1 S c f) where
    gparse = M1 <$> gparse
