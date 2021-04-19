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

    , mapValue
    , traverseValue
    ) where

import qualified Data.ByteString.Lazy  as LBS
import           Data.Fix              (Fix(..))
import           Data.Functor.Classes  (Eq1(..), Ord1(..))
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import qualified Data.Text.Lazy        as LT
import           GHC.Generics          (Generic)
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

data DecodeError = DecodeError

class EncodePointers p m where
    encodePointers :: Value p -> m (Fix Value)

class ToValue a p where
    toValue :: a -> (Value p)

class FromValue a p where
    fromValue :: Value p -> Either DecodeError a

-- TODO: make sure Ord instance agrees with the spec -- right now we depend
-- on the `containers` package's notion of order, which I(zenhack) haven't
-- checked is the same.

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

mapValue :: (Ord a, Ord b) => (a -> b) -> Value a -> Value b
mapValue f = runIdentity . traverseValue (pure . f)

traverseValue :: Applicative f => (Ord a, Ord b) => (a -> f b) -> Value a -> f (Value b)
traverseValue f = \case
    Atom x -> pure $ Atom x
    Compound x -> Compound <$> case x of
        Record tag args -> Record <$> go tag <*> (traverse go args)
        Sequence xs     -> Sequence <$> traverse go xs
        Set xs          -> Set . S.fromList <$> traverse go (S.toList xs)
        Dictionary xs   -> Dictionary . M.fromList <$>
            for (M.toList xs) (\(k, v) -> (,) <$> go k <*> go v)
    Pointer p       -> Pointer <$> f p
  where
    go = traverseValue f

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
