module Ergvein.Filters.Hash(
    hashToRange
  , hashSetConstruct
  , SipKey
  ) where

import Data.ByteArray.Hash
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Word
import Data.Bits

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- | Hash data to range from [0, F)
hashToRange :: Word64 -- ^ Maximum range boundary F
  -> SipKey -- ^ Key for hash function
  -> ByteString -- ^ Payload to hash
  -> Word64 -- ^ Resulted hash
hashToRange rangeF key bs = a * c + h(a * d + c * b + h(b * d))
  where
    SipHash hash = sipHash key bs
    l n = n .&. 0xffffffff
    h n = n `shiftR` 32
    a = h hash
    b = l hash
    c = h rangeF
    d = l rangeF

-- | Construct hashes that are added to filter.
hashSetConstruct :: SipKey -- ^ Key for hash function
  -> Word64 -- ^ False positive factor. Matches other items with probability 1 / M
  -> Vector ByteString -- ^ Payload items
  -> VU.Vector Word64 -- ^ Resulted hashes
hashSetConstruct key m xs = V.convert $ fmap (hashToRange (n * m) key) xs
  where
    n = fromIntegral $ length xs
