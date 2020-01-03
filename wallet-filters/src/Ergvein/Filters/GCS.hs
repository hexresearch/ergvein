-- | Module to work with Golomb-coded sets (GCS).
--
-- For each block, compact filters are derived containing sets of items associated with the block (eg. addresses sent to, outpoints spent, etc.). A set of such data objects is compressed into a probabilistic structure called a Golomb-coded set (GCS), which matches all items in the set with probability 1, and matches other items with probability 1/M for some integer parameter M. The encoding is also parameterized by P, the bit length of the remainder code. Each filter defined specifies values for P and M.
--
-- At a high level, a GCS is constructed from a set of N items by:
--
-- * hashing all items to 64-bit integers in the range [0, N * M)
--
-- * sorting the hashed values in ascending order
--
-- * computing the differences between each value and the previous one
--
-- * writing the differences sequentially, compressed with Golomb-Rice coding
module Ergvein.Filters.GCS(
    constructGcs
  ) where

import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Word
import Ergvein.Filters.Hash

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Heap as V
import qualified Data.Encoding.GolombRice.Strict as G

-- | Construct Golomb-coded set for filters.
--
-- The raw items in L are first hashed to 64-bit unsigned integers as specified
-- above and sorted. The differences between consecutive values, hereafter
-- referred to as deltas, are encoded sequentially to a bit stream with
-- Golomb-Rice coding. Finally, the bit stream is padded with 0's to the
-- nearest byte boundary and serialized to the output byte vector.
--
-- The result is a byte vector with a minimum size of N * (P + 1) bits.
constructGcs :: Int -- ^ the bit P parameter of the Golomb-Rice coding
  -> SipKey -- ^ k the 128-bit key used to randomize the SipHash outputs
  -> Word64 -- ^ M the target false positive rate
  -> Vector ByteString -- ^ Elements L that we need to add to filter. Length N
  -> ByteString -- ^ Compressed set
constructGcs p k m ls = G.toByteString gs
  where
    is = hashSetConstruct k m ls
    iss = runST $ do
      mv <- VU.unsafeThaw is
      V.sort mv
      VU.unsafeFreeze mv
    ids = VU.zipWith (-) iss (VU.cons 0 iss)
    gs = G.fromVectorUnboxed p ids :: G.GolombRice Word64
