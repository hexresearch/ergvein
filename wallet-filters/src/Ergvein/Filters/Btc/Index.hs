-- | Implements BIP-158 like filter for Bech32 addresses. Note
-- that IT IS NOT exact BIP-158 as we don't put all public and redeem scripts
-- inside the filter to save bandwidth.
{-# LANGUAGE BangPatterns #-}
module Ergvein.Filters.Btc.Index(
  -- * Constants for BIP158
    btcDefP
  , btcDefM
  -- * Filter hash
  , FilterHash(..)
  , filterHashFromText
  , filterHashToText
  , preGenesisFilterHash
  -- * Outpoint quering context
  , HasTxIndex(..)
  , withInputTxs
  , foldInputs
  -- * Filtering elements of filter
  , isBip158Indexable
  , isErgveinIndexable
    -- * SegWit address helpers
  , addressToScriptBS
    -- * Siphash
  , blockSipHash
  ) where

import Control.Monad.Reader
import Data.ByteArray.Hash ( SipKey(..) )
import Data.ByteString ( ByteString )
import Data.Map.Strict (Map)
import Data.Serialize ( encode )
import Data.Text (Text)
import Data.Word
import Ergvein.Text
import Network.Haskoin.Address
import Network.Haskoin.Block
import Network.Haskoin.Script
import Network.Haskoin.Transaction

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS

import Debug.Trace

-- | Default value for P parameter (amount of bits in golomb rice encoding).
-- Set to fixed `19` according to BIP-158.
btcDefP :: Int
btcDefP = 19

-- | Default value for M parameter (the target false positive rate).
-- Set to fixed `784931` according to BIP-158.
btcDefM :: Word64
btcDefM = 784931

-- | Filter hash is id that is calculated as hash from filter data and hash of previous filter
newtype FilterHash = FilterHash { unFilterHash :: ByteString }
  deriving (Eq, Ord, Show)

-- | Decode hex encoded filter hash
filterHashFromText :: Text -> FilterHash
filterHashFromText = FilterHash . hex2bs

-- | Encode filter hash as hex
filterHashToText :: FilterHash -> Text
filterHashToText = bs2Hex . unFilterHash

-- | Special hash for filter before genesis block
preGenesisFilterHash :: FilterHash
preGenesisFilterHash = filterHashFromText "0000000000000000000000000000000000000000000000000000000000000000"

-- | Creation of filters require quering of each output script for each input tx,
-- so you need provide  a way `makeBtcFilter` can query them.
class Monad m => HasTxIndex m where
  queryOutPoint :: OutPoint -> m (Maybe ByteString)

instance Monad m => HasTxIndex (ReaderT (Map OutPoint ByteString) m) where
  queryOutPoint i = do
    m <- ask
    pure $ M.lookup i m
  {-# INLINE queryOutPoint #-}

-- | Execute context with given list of transactions as input. Used for testing
-- filters with already known set of input transactions.
withInputTxs :: [Tx] -> (ReaderT (Map OutPoint ByteString) m a) -> m a
withInputTxs txs = flip runReaderT txmap
  where
    txmap :: Map OutPoint ByteString
    txmap = M.fromList $ concat $ fmap (\tx -> fmap (\(out, i) -> (OutPoint (txHash tx) i, scriptOutput out)) $ txOut tx `zip` [0 ..]) txs

-- | Iterate over all inputs and collect corresponding outputs
foldInputs :: forall a m . HasTxIndex m => (a -> ByteString -> m a) -> a -> Block -> m a
foldInputs f a0 block = go a0 $ fmap prevOutput . concat . fmap txIn . drop 1 . blockTxns $ block
  where
    go :: a -> [OutPoint] -> m a
    go !a [] = pure a
    go !a (o : os) = do
      mbs <- queryOutPoint o
      case mbs of
        Nothing -> go a os
        Just bs -> do
          a' <- f a bs
          go a' os

-- | Standard indexes all but data carriers.
isBip158Indexable :: ByteString -> Bool
isBip158Indexable bs = kindCheck && not (BS.null bs)
  where
    kindCheck = case decodeOutputBS bs of
      Right (DataCarrier          {}) -> False
      _                               -> True
{-# INLINE isBip158Indexable #-}

-- | We index only segwit scripts, multisigs and data carrier for USDT support.
isErgveinIndexable :: ByteString -> Bool
isErgveinIndexable bs = kindCheck && not (BS.null bs)
  where
    kindCheck = case decodeOutputBS bs of
      Right (PayWitnessPKHash     {}) -> True
      Right (PayWitnessScriptHash {}) -> True
      Right (PayMulSig            {}) -> True
      Right (DataCarrier          {}) -> True
      _                               -> False
{-# INLINE isErgveinIndexable #-}

-- | Siphash key for filter is first 16 bytes of the hash (in standard little-endian representation)
-- of the block for which the filter is constructed. This ensures the key is deterministic while
-- still varying from block to block.
blockSipHash :: BlockHash -> SipKey
blockSipHash bh = fromBs . encode . getBlockHash $ bh
 where
  toWord64 =
    fst . foldl (\(!acc, !i) b -> (acc + fromIntegral b * (256 ^ i), i + (1 :: Word64))) (0, 0)
  k1 bs = toWord64 $ BS.unpack . BS.take 8 $ bs
  k2 bs = toWord64 $ BS.unpack . BS.take 8 . BS.drop 8 $ bs
  fromBs bs = SipKey (k1 bs) (k2 bs)
