module Preserves.Syrup.Decode
    ( getValue
    ) where

import           Data.Binary.Get
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy     as LT
import           Preserves
import           Zhp

getValue :: Ord a => Get (Value a)
getValue = do
    b <- lookAhead getWord8
    case toEnum (fromIntegral b) of
        't' -> getWord8 *> pure (Atom $ Bool True)
        'f' -> getWord8 *> pure (Atom $ Bool False)
        'F' -> getWord8 *> (Atom . Float <$> getFloatbe)
        'D' -> getWord8 *> (Atom . Double <$> getDoublebe)
        'i' -> getWord8 *> (Atom . SignedInteger <$> getSignedInteger)
        '{' -> Compound . Dictionary <$> getDictionary
        '[' -> Compound . Sequence <$> getSequence
        '#' -> Compound . Set <$> getSet
        _   -> Atom <$> getStringLike

getStringLike :: Get Atom
getStringLike = do
    (n, c) <- getInteger
    case c of
        ':'  -> ByteString <$> getLazyByteString (fromIntegral n)
        '"'  -> String <$> getString (fromIntegral n)
        '\'' -> Symbol <$> getString (fromIntegral n)
        _    -> empty

getString :: Int -> Get LT.Text
getString len = do
    bytes <- getByteString len
    case TE.decodeUtf8' bytes of
        Right v -> pure (LT.fromChunks [v])
        Left _e -> empty

getAnyChar8 :: Get Char
getAnyChar8 = toEnum . fromIntegral <$> getWord8

getChar8 :: Char -> Get ()
getChar8 c = do
    b <- getAnyChar8
    if b == c
        then pure ()
        else empty

between :: Get a -> Get b -> Get c -> Get [c]
between start end elts = do
    _ <- start
    go
  where
    go = asum
        [ end *> pure []
        , (:) <$> elts <*> go
        ]

getSequence :: Ord a => Get [Value a]
getSequence = between (getChar8 '[') (getChar8 ']') getValue

getSet :: Ord a => Get (S.Set (Value a))
getSet = S.fromList <$> between (getChar8 '#') (getChar8 '$') getValue

getDictionary :: Ord a => Get (M.Map (Value a) (Value a))
getDictionary = M.fromList <$> between (getChar8 '{') (getChar8 '}')
    ((,) <$> getValue <*> getValue)

parseDigit :: Char -> Integer
parseDigit c = fromIntegral (fromEnum c - fromEnum '0')

getInteger :: Get (Integer, Char)
getInteger = do
    c <- getAnyChar8
    if isDigit c
        then go (parseDigit c)
        else empty
  where
    go accum = do
        c <- getAnyChar8
        if isDigit c
            then go (accum * 10 + parseDigit c)
            else pure (accum, c)

getSignedInteger :: Get Integer
getSignedInteger = do
    c <- lookAhead getAnyChar8
    case c of
        '+' -> getAnyChar8 *> go
        '-' -> getAnyChar8 *> (negate <$> go)
        _   -> go
  where
    go = do
        (n, e) <- getInteger
        if e == 'e'
            then pure n
            else empty
