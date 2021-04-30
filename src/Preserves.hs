{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Preserves
    ( Atom(..)
    , Value(..)
    , Compound(..)
    , ToValue(..)
    , FromValue(..)
    , EncodePointers(..)
    , interpPtrs
    , Anno(..)
    , stripAnno
    ) where

import qualified Data.ByteString.Lazy  as LBS
import           Data.Fix              (Fix(..))
import           Data.Functor.Classes
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import qualified Data.Text.Lazy        as LT
import           GHC.Generics          (Generic)
import           Text.Show             (showListWith, showParen, showString)
import           Zhp                   hiding (map)

data Atom
    = Bool !Bool
    | Float !Float
    | Double !Double
    | SignedInteger !Integer
    | String !LT.Text
    | ByteString !LBS.ByteString
    | Symbol !LT.Text
    deriving(Show, Read, Eq, Ord, Generic)

data Compound p
    = Record (Value p) [Value p]
    | Sequence [Value p]
    | Set (S.Set (Value p))
    | Dictionary (M.Map (Value p) (Value p))
    deriving(Show, Read, Eq, Ord, Generic)

data Value p
    = Atom !Atom
    | Compound (Compound p)
    | Pointer p
    deriving(Show, Read, Eq, Ord, Generic)

data Anno p
    = AnnoValue [Value (Anno p)] (Value (Anno p))
    | AnnoPtr p
    deriving(Show, Read, Generic)

instance Ord p => Eq (Anno p) where
    x == y = stripAnno x == stripAnno y

instance Ord p => Ord (Anno p) where
    compare x y = compare (stripAnno x) (stripAnno y)

stripAnno :: Ord p => Anno p -> Value p
stripAnno = runIdentity . go
  where
    go = \case
        AnnoValue _ v -> interpPtrs go v
        AnnoPtr p     -> pure $ Pointer p

data DecodeError = DecodeError

class EncodePointers p m where
    encodePointers :: Value p -> m (Fix Value)

class ToValue a p where
    toValue :: a -> (Value p)

class FromValue a p where
    fromValue :: Value p -> Either DecodeError a

instance Show1 Compound where
    liftShowsPrec f fl p v =
        let go = liftShowsPrec f fl in
        case v of
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
                EQ  -> liftCompare (liftCompare f) argsx argsy
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
toListList m = [ [k, v]  | (k, v) <- M.toList m ]

instance Show1 Value where
    liftShowsPrec f fl p = \case
        Atom v    -> showParen (p > 10) $ showString "Atom " . showsPrec 11 v
        Pointer v -> showParen (p > 10) $ showString "Pointer " . f 11 v
        Compound v -> showParen (p > 10) $ showString "Compound " . liftShowsPrec f fl 11 v

instance Eq1 Value where
    liftEq f x y = case (x, y) of
        (Atom x', Atom y')         -> x' == y'
        (Compound x', Compound y') -> liftEq f x' y'
        (Pointer x', Pointer y')   -> f x' y'
        _                          -> False

instance Ord1 Value where
    liftCompare f x y = case (x, y) of
        (Atom x', Atom y')         -> compare x' y'
        (Compound x', Compound y') -> liftCompare f x' y'
        (Pointer x', Pointer y')   -> f x' y'

        (Atom _, _)                -> LT
        (_, Atom _)                -> GT
        (Compound _, _)            -> LT
        (_, Compound _)            -> GT

interpPtrs :: (Applicative f, Ord a, Ord b) => (a -> f (Value b)) -> Value a -> f (Value b)
interpPtrs interp = \case
    Atom a     -> pure $ Atom a
    Pointer p  -> interp p
    Compound c -> Compound <$> traverseCompound (interpPtrs interp) c

traverseCompound :: (Applicative f, Ord a, Ord b) => (Value a -> f (Value b)) -> Compound a -> f (Compound b)
traverseCompound f = \case
    Record label args ->
        Record
            <$> f label
            <*> traverse f args
    Sequence xs ->
        Sequence <$> traverse f xs
    Set xs ->
        Set . S.fromList <$> traverse f (S.toList xs)
    Dictionary d ->
        Dictionary . M.fromList <$>
            sequence [ (,) <$> f k <*> f v | (k, v) <- M.toList d ]

instance ToValue (Value p) p where
    toValue = id

instance FromValue (Value p) p where
    fromValue = pure

instance ToValue Atom p where
    toValue = Atom

instance FromValue Atom p where
    fromValue (Atom v) = Right v
    fromValue _        = Left DecodeError

instance ToValue (Compound p) p where
    toValue = Compound

instance FromValue (Compound p) p where
    fromValue (Compound c) = Right c
    fromValue _            = Left DecodeError
