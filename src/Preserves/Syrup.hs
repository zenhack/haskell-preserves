module Preserves.Syrup
    ( pureValueToBuilder
    ) where

import Preserves
import Zhp

import qualified Data.ByteString.Builder as BB

pureValueToBuilder :: Fix Value -> BB.Builder
pureValueToBuilder (Fix v) = case v of
    _ -> error "TODO"
