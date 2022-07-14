{-# LANGUAGE LambdaCase #-}
module Preserves.Syrup.Encode
    ( encodeValue
    ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LBS
import           Data.Fix
import qualified Data.Map.Strict         as M
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as LT
import           Preserves
import           Zhp

encodeValue :: Value (Fix Value) -> BB.Builder
encodeValue = \case
    Atom a           -> encodeAtom a
    Compound c       -> encodeCompound c
    Embedded (Fix p) -> "!" <> encodeValue p

encodeAtom :: Atom -> BB.Builder
encodeAtom = \case
    Bool True       -> "t"
    Bool False      -> "f"
    Float n         -> "F" <> BB.floatBE n
    Double n        -> "D" <> BB.doubleBE n
    SignedInteger n
        | n < 0 -> fromString (show (negate n)) <> "-"
        | otherwise -> fromString (show n) <> "+"
    String s        -> encodeText "\"" s
    ByteString lbs  -> encodeBytes ":" lbs
    Symbol s        -> encodeText "'" s

encodeCompound :: Compound (Fix Value) -> BB.Builder
encodeCompound = \case
    Record tag args -> "<" <> foldMap encodeAnno (tag:args) <> ">"
    Sequence xs -> "[" <> foldMap encodeAnno xs <> "]"
    Set xs -> "#" <> foldMap encodeAnno xs <> "$"
    Dictionary xs -> "{" <> foldMap (\(k, v) -> encodeAnno k <> encodeAnno v) (M.toList xs) <> "}"

encodeAnno :: Anno (Fix Value) -> BB.Builder
encodeAnno (Anno _ v) = encodeValue v

encodeBytes :: BB.Builder -> LBS.ByteString -> BB.Builder
encodeBytes joiner lbs = fromString (show $ LBS.length lbs) <> joiner <> BB.lazyByteString lbs

encodeText :: BB.Builder -> LT.Text -> BB.Builder
encodeText joiner s =  encodeBytes joiner $ LBS.fromChunks $ map TE.encodeUtf8 $ LT.toChunks s
