{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeOperators #-}

module Hakase.Render where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word32)
import GHC.Generics
import Text.Printf (printf)


class Render a where
    render :: a -> ByteString
    default render :: (Generic a, GRender (Rep a)) => a -> ByteString
    render = grender . from

instance Render Word32 where
    render = render . BC.pack . show

instance Render Text where
    render = render . Text.encodeUtf8

instance Render ByteString where
    render s
        | BC.length s <= 99 = len <> s
        | otherwise = error "render: string too long"
      where
        len = BC.pack $ printf "%02d" (BC.length s)


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
    grender (K1 c) = render c

instance forall c f. (GRender f, Constructor c) => GRender (C1 c f) where
    grender (M1 f) = render (BC.pack name) <> grender f
      where
        name = map toLower $ conName (undefined :: C1 c f ())

instance GRender f => GRender (D1 c f) where
    grender (M1 f) = grender f

instance GRender f => GRender (S1 c f) where
    grender (M1 f) = grender f
