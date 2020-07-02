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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module Ergvein.Filters.GCS.Mutable(
    GCS
  , SipKey
  , encodeGcs
  , decodeGcs
  , constructGcs
  , matchGcs
  , matchGcsMany
  ) where

import Control.Monad.IO.Class
import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Word
import Ergvein.Filters.Hash

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Heap as V
import qualified Data.Encoding.GolombRice.Strict.Mutable as G

-- | Unserialised Golomb-coded set.
type GCS = G.GolombRice Word64

-- | The bit stream is padded with 0's to the nearest byte
-- boundary and serialized to the output byte vector.
encodeGcs :: MonadIO m => GCS -> m ByteString
encodeGcs = G.toByteString

-- | Decoding GCS from byte vector.
decodeGcs :: MonadIO m
  => Int -- ^ the bit P parameter of the Golomb-Rice coding
  -> ByteString -> m GCS
decodeGcs p = G.fromByteString p

-- | Construct Golomb-coded set for filters.
--
-- The raw items in L are first hashed to 64-bit unsigned integers as specified
-- above and sorted. The differences between consecutive values, hereafter
-- referred to as deltas, are encoded sequentially to a bit stream with
-- Golomb-Rice coding.
--
-- The result is a byte vector with a minimum size of N * (P + 1) bits.
constructGcs :: MonadIO m
  => Int -- ^ the bit P parameter of the Golomb-Rice coding
  -> SipKey -- ^ k the 128-bit key used to randomize the SipHash outputs
  -> Word64 -- ^ M the target false positive rate
  -> Vector ByteString -- ^ Elements L that we need to add to filter. Length N
  -> m GCS
constructGcs p k m ls = G.fromVectorUnboxed p ids
  where
    is = hashSetConstruct k m ls
    iss = runST $ do
      mv <- VU.unsafeThaw is
      V.sort mv
      VU.unsafeFreeze mv
    ids = VU.zipWith (-) iss (VU.cons 0 iss)

-- | To check membership of an item in a compressed GCS, one must reconstruct
-- the hashed set members from the encoded deltas. The procedure to do so is
-- the reverse of the compression: deltas are decoded one by one and added to a
-- cumulative sum. Each intermediate sum represents a hashed value in the original
-- set. The queried item is hashed in the same way as the set members and compared
-- against the reconstructed values. Note that querying does not require the
-- entire decompressed set be held in memory at once.
--
-- Note: the filter is consumed after the operation. Use 'matchGcsMany' to match
-- against several targets at once.
matchGcs :: MonadIO m
  => Int -- ^ the bit P parameter of the Golomb-Rice coding
  -> SipKey -- ^ k the 128-bit key used to randomize the SipHash outputs
  -> Word64 -- ^ M the target false positive rate
  -> Word64 -- ^ N the total amount of items in set
  -> GCS -- ^ Filter set
  -> ByteString -- ^ Target to test against Gcs
  -> m Bool
matchGcs p k m n gcs target = fmap fst $ G.foldl f (False, 0) gcs
  where
    targetHash = hashToRange (n * m) k target
    f (!_, !lastValue) delta = pure $ let
      setItem = lastValue + delta
      in if | setItem == targetHash -> G.Stop (True, setItem)
            | setItem > targetHash -> G.Stop (False, setItem)
            | otherwise -> G.Next (False, setItem)

-- | Same as `matchGcs` but allows to check several targets against the GCS.
-- Note that the filter is consumed after the operation.
matchGcsMany :: (MonadIO m)
  => Int -- ^ the bit P parameter of the Golomb-Rice coding
  -> SipKey -- ^ k the 128-bit key used to randomize the SipHash outputs
  -> Word64 -- ^ M the target false positive rate
  -> Word64 -- ^ N the total amount of items in set
  -> GCS -- ^ Filter set
  -> [ByteString] -- ^ Targets to test against Gcs
  -> m Bool
matchGcsMany p k m n gcs targets = fmap (\(v,_,_)->v) $ G.foldl f (False, targetHashes, 0) gcs
  where
    targetHashes = fmap (hashToRange (n * m) k) targets
    f (!_, !hs, !lastValue) delta = pure $ let
      setItem = lastValue + delta
      hs' = filter (setItem <) hs
      in if | any (setItem ==) hs -> G.Stop (True, hs, setItem)
            | null hs' -> G.Stop (False, hs, setItem)
            | otherwise -> G.Next (False, hs', setItem)
