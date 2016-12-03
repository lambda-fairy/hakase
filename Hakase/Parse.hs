{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}

module Hakase.Parse where


import Control.Applicative ((<|>), empty)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word32)
import GHC.Generics
import Text.Printf (printf)
import Text.Read (readMaybe)


class Parse a where
    parse :: Parser a
    default parse :: (Generic a, GParse (Rep a)) => Parser a
    parse = to <$> gparse

instance Parse Word32 where
    parse = parse >>= maybe empty return . readMaybe . BC.unpack

instance Parse Text where
    parse = parse >>= either (fail . show) return . Text.decodeUtf8'

instance Parse ByteString where
    parse = do
        n <- P.take 2 >>= maybe empty return . readMaybe . BC.unpack
        P.take n


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
    gparse = K1 <$> parse

instance forall c f. (GParse f, Constructor c) => GParse (C1 c f) where
    gparse = M1 <$ P.stringCI conString <*> gparse
      where
        conString = BC.pack $ printf "%02d" (length name) ++ name
        name = conName (undefined :: C1 c f ())

instance GParse f => GParse (D1 c f) where
    gparse = M1 <$> gparse

instance GParse f => GParse (S1 c f) where
    gparse = M1 <$> gparse
