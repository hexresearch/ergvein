module Ergvein.Types.Fees
  (
    FeeLevel(..)
  , FeeBundle(..)
  , extractFee
  , mkFeeBundle
  , feeTargetBlocks
  ) where

import Data.List (foldl')
import Data.Word
import Ergvein.Aeson
import Ergvein.Types.Currency

data FeeLevel = FeeFast | FeeModerate | FeeCheap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

feeTargetBlocks :: Currency -> FeeLevel -> Int
feeTargetBlocks cur lev = case (cur, lev) of
  (BTC, FeeFast)      -> 2
  (BTC, FeeModerate)  -> 10
  (BTC, FeeCheap)     -> 25

-- Tuples for smartFee: Conservative and Economical
data FeeBundle = FeeBundle {
  feeBundle'high :: !(Word64, Word64)
, feeBundle'mid  :: !(Word64, Word64)
, feeBundle'low  :: !(Word64, Word64)
} deriving (Eq, Show)

$(deriveJSON aesonOptionsStripToApostroph ''FeeBundle)

extractFee :: FeeLevel -> FeeBundle -> (Word64, Word64)
extractFee lvl FeeBundle{..} = case lvl of
  FeeFast     -> feeBundle'high
  FeeModerate -> feeBundle'mid
  FeeCheap    -> feeBundle'low

mkFeeBundle :: [(FeeLevel, (Word64, Word64))] -> FeeBundle
mkFeeBundle = foo def $ \b (lvl, v) -> case lvl of
  FeeFast     -> b {feeBundle'high = v}
  FeeModerate -> b {feeBundle'mid = v}
  FeeCheap    -> b {feeBundle'low = v}
  where
    def = FeeBundle (0,0) (0,0) (0,0)
    foo b f ta = foldl' f b ta
