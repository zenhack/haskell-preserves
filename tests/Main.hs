{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Data.Binary.Get
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LBS
import           Data.Fix
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import qualified Data.Text.Lazy          as LT
import           Hedgehog
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range
import           Preserves
import           Preserves.Syrup
import           Zhp

main :: IO ()
main = do
    ok <- checkParallel $$(discover)
    unless ok $ exitFailure

genValue :: MonadGen m => m (Value (Fix Value))
genValue = Gen.choice
    [ Atom <$> genAtom
    , Compound <$> genCompound
    , Embedded . Fix <$> Gen.small genValue
    ]

text :: MonadGen m => m LT.Text
text = LT.fromStrict <$> Gen.text (Range.linear 0 20) Gen.unicode

genAtom :: MonadGen m => m Atom
genAtom = Gen.choice
    [ Bool <$> Gen.bool
    , Float <$> Gen.float (Range.exponentialFloat (-100) 100)
    , Double <$> Gen.double (Range.exponentialFloat (-100) 100)
    , SignedInteger <$> Gen.integral (Range.linear (-10000000000000000) 10000000000000000)
    , String <$> text
    , ByteString . LBS.fromStrict <$> Gen.bytes (Range.linear 0 200)
    , Symbol <$> text
    ]

genList :: MonadGen m => m a -> m [a]
genList = Gen.list (Range.linear 0 10)

genValues :: MonadGen m => m [Value (Fix Value)]
genValues = genList (Gen.small genValue)

genCompound :: MonadGen m => m (Compound (Fix Value))
genCompound = Gen.choice
    [ Record <$> Gen.small genValue <*> genValues
    , Sequence <$> genValues
    , Set . S.fromList <$> genValues
    , Dictionary . M.fromList <$> genList (Gen.small $ (,) <$> genValue <*> genValue)
    ]

prop_syrupEncodeDecodeId :: Property
prop_syrupEncodeDecodeId = property $ do
    value <- forAll genValue
    let bb     = encodeValue value
        value' = runGet getValue (BB.toLazyByteString bb)
    value === value'
