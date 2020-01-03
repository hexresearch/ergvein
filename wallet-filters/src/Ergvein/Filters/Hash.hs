module Ergvein.Filters.Hash(
    hashToRange
  , hashSetConstruct
  , SipKey
  ) where

import Data.ByteArray.Hash
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.WideWord.Word128
import Data.Word

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- | Hash data to range from [0, F)
hashToRange :: Word64 -- ^ Maximum range boundary F
  -> SipKey -- ^ Key for hash function
  -> ByteString -- ^ Payload to hash
  -> Word64 -- ^ Resulted hash
hashToRange rangeF key bs = hf
  where
    SipHash h = sipHash key bs
    hf = word128Hi64 $ fromIntegral h * fromIntegral rangeF

-- | Construct hashes that are added to filter.
hashSetConstruct :: SipKey -- ^ Key for hash function
  -> Word64 -- ^ False positive factor. Matches other items with probability 1 / M
  -> Vector ByteString -- ^ Payload items
  -> VU.Vector Word64 -- ^ Resulted hashes
hashSetConstruct key m xs = V.convert $ fmap (hashToRange (n * m) key) xs
  where
    n = fromIntegral $ length xs
