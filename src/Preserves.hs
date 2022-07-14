{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Preserves
  ( Atom (..),
    Value (..),
    Compound (..),
    ToValue (..),
    FromValue (..),
    EncodeEmbedded (..),
    interpEmbedded,
    Anno (..),
  )
where

import qualified Data.ByteString.Lazy as LBS
import Data.Fix (Fix (..))
import Data.Functor.Classes
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import GHC.Generics (Generic)
import Text.Show (showListWith, showParen, showString)
import Zhp hiding (map)

data Atom
  = Bool !Bool
  | Float !Float
  | Double !Double
  | SignedInteger !Integer
  | String !LT.Text
  | ByteString !LBS.ByteString
  | Symbol !LT.Text
  deriving (Show, Read, Eq, Ord, Generic)

data Compound e
  = Record (Anno e) [Anno e]
  | Sequence [Anno e]
  | Set (S.Set (Anno e))
  | Dictionary (M.Map (Anno e) (Anno e))
  deriving (Show, Read, Eq, Ord, Generic)

data Value e
  = Atom !Atom
  | Compound (Compound e)
  | Embedded e
  deriving (Show, Read, Eq, Ord, Generic)

data Anno e = Anno
  { annotations :: [Value e],
    annoValue :: Value e
  }
  deriving (Show, Read, Generic)

instance Ord p => Eq (Anno p) where
  x == y = annoValue x == annoValue y

instance Ord p => Ord (Anno p) where
  compare x y = compare (annoValue x) (annoValue y)

data DecodeError = DecodeError

class EncodeEmbedded e m where
  encodeEmbedded :: e -> m (Fix Value)

class ToValue a e where
  toValue :: a -> Value e

class FromValue a e where
  fromValue :: Value e -> Either DecodeError a

instance Show1 Compound where
  liftShowsPrec f fl p v =
    let go = liftShowsPrec f fl
     in case v of
          Record x xs ->
            showParen (p > 10) $
              showString "Record "
                . go 11 x
                . showListWith (go 0) xs
          Sequence xs ->
            showParen (p > 10) $
              showString "Sequence " . showListWith (go 0) xs
          Set xs ->
            showParen (p > 10) $
              showString "Set (fromList " . showListWith (go 0) (S.toList xs) . showString ")"
          Dictionary xs ->
            showParen (p > 10) $
              showString "Dictionary (fromList "
                . showListWith
                  (\(x, y) -> showString "(" . go 0 x . showString "," . go 0 y . showString ")")
                  (M.toList xs)
                . showString ")"

instance Ord1 Anno where
  liftCompare f x y = liftCompare f (annoValue x) (annoValue y)

instance Eq1 Anno where
  liftEq f x y = liftEq f (annoValue x) (annoValue y)

instance Show1 Anno where
  liftShowsPrec f fl p v =
    let go = liftShowsPrec f fl
     in showParen (p > 10) $
          showString "Anno "
            . showListWith (go 0) (annotations v)
            . go 11 (annoValue v)

instance Eq1 Compound where
  liftEq f x y = case (x, y) of
    (Record tx argsx, Record ty argsy) ->
      (liftEq f tx ty) && (liftEq (liftEq f) argsx argsy)
    (Sequence xs, Sequence ys) -> liftEq (liftEq f) xs ys
    (Set xs, Set ys) -> liftEq (liftEq f) xs ys
    (Dictionary xs, Dictionary ys) ->
      liftEq
        (liftEq (liftEq f))
        (toListList xs)
        (toListList ys)
    (_, _) -> False

-- Implement type classes from Data.Functor.Classes. Annoyingly, there doesn't
-- seem to be a good way to auto-derive these; the fact that we use the parameter
-- as a map key means that it always has an Ord constraint, which breaks everything.
-- so the implementations below are the boring ones you'd get if you could just derive
-- them, but we have to do them by hand...
--
-- TODO: make sure the Ord{,1} instance agrees with the spec -- right now we depend
-- on the `containers` package's notion of order, which I(zenhack) haven't
-- checked is the same.

instance Ord1 Compound where
  liftCompare f x y = case (x, y) of
    (Record tx argsx, Record ty argsy) ->
      case liftCompare f tx ty of
        EQ -> liftCompare (liftCompare f) argsx argsy
        res -> res
    (Sequence xs, Sequence ys) ->
      liftCompare (liftCompare f) xs ys
    (Set xs, Set ys) ->
      liftCompare (liftCompare f) xs ys
    (Dictionary xs, Dictionary ys) ->
      liftCompare
        (liftCompare (liftCompare f))
        (toListList xs)
        (toListList ys)
    (Record _ _, _) -> LT
    (_, Record _ _) -> GT
    (Sequence _, _) -> LT
    (_, Sequence _) -> GT
    (Set _, _) -> LT
    (_, Set _) -> GT

toListList :: M.Map k k -> [[k]]
toListList m = [[k, v] | (k, v) <- M.toList m]

instance Show1 Value where
  liftShowsPrec f fl p = \case
    Atom v -> showParen (p > 10) $ showString "Atom " . showsPrec 11 v
    Embedded v -> showParen (p > 10) $ showString "Embedded " . f 11 v
    Compound v -> showParen (p > 10) $ showString "Compound " . liftShowsPrec f fl 11 v

instance Eq1 Value where
  liftEq f x y = case (x, y) of
    (Atom x', Atom y') -> x' == y'
    (Compound x', Compound y') -> liftEq f x' y'
    (Embedded x', Embedded y') -> f x' y'
    _ -> False

instance Ord1 Value where
  liftCompare f x y = case (x, y) of
    (Atom x', Atom y') -> compare x' y'
    (Compound x', Compound y') -> liftCompare f x' y'
    (Embedded x', Embedded y') -> f x' y'
    (Atom _, _) -> LT
    (_, Atom _) -> GT
    (Compound _, _) -> LT
    (_, Compound _) -> GT

interpEmbedded :: (Applicative f, Ord a, Ord b) => (a -> f (Value b)) -> Value a -> f (Value b)
interpEmbedded interp = \case
  Atom a -> pure $ Atom a
  Embedded p -> interp p
  Compound c -> Compound <$> traverseCompound (interpEmbedded interp) c

traverseCompound :: (Applicative f, Ord a, Ord b) => (Value a -> f (Value b)) -> Compound a -> f (Compound b)
traverseCompound f = \case
  Record label args ->
    Record
      <$> fa label
      <*> traverse fa args
  Sequence xs ->
    Sequence <$> traverse fa xs
  Set xs ->
    Set . S.fromList <$> traverse fa (S.toList xs)
  Dictionary d ->
    Dictionary . M.fromList
      <$> sequence [(,) <$> fa k <*> fa v | (k, v) <- M.toList d]
  where
    fa = traverseAnno f

traverseAnno :: (Applicative f, Ord a, Ord b) => (Value a -> f (Value b)) -> Anno a -> f (Anno b)
traverseAnno f (Anno a v) = Anno <$> traverse f a <*> f v

instance ToValue (Value e) e where
  toValue = id

instance FromValue (Value e) e where
  fromValue = pure

instance ToValue Atom e where
  toValue = Atom

instance FromValue Atom e where
  fromValue (Atom v) = Right v
  fromValue _ = Left DecodeError

instance ToValue (Compound e) e where
  toValue = Compound

instance FromValue (Compound e) e where
  fromValue (Compound c) = Right c
  fromValue _ = Left DecodeError
