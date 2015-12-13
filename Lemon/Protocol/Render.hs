{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}

module Lemon.Protocol.Render where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics

import Lemon.Protocol.Types


renderMessage :: Message -> ByteString
renderMessage = grender . from


class Render a where
    render :: a -> ByteString

instance Render Word32 where
    render = BC.pack . show

instance Render Text where
    render = T.encodeUtf8

instance Render ByteString where
    render = id

instance Render Player where
    render p = case p of
        White -> "0"
        Black -> "1"

instance Render Move where
    render m = case m of
        Rock -> "0"
        Paper -> "1"
        Scissors -> "2"

instance Render Error where
    render (KnownError e) = BC.pack $ show e
    render (OtherError s) = T.encodeUtf8 s


class GRender f where
    grender :: f p -> ByteString

instance GRender V1 where
    grender = undefined

instance GRender U1 where
    grender = mempty

instance (GRender f, GRender g) => GRender (f :+: g) where
    grender (L1 f) = grender f
    grender (R1 g) = grender g

instance (GRender f, GRender g) => GRender (f :*: g) where
    grender (f :*: g) = grender f <> grender g

instance Render c => GRender (K1 i c) where
    grender (K1 c) = ":" <> render c

instance forall c f. (GRender f, Constructor c) => GRender (C1 c f) where
    grender (M1 f) = name <> grender f
      where
        name = BC.pack $ conName (undefined :: C1 c f ())

instance GRender f => GRender (M1 D c f) where
    grender (M1 f) = grender f

instance GRender f => GRender (M1 S c f) where
    grender (M1 f) = grender f
