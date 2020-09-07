-- | Implements BIP-158 like filter for Bech32 addresses. Note
-- that IT IS NOT exact BIP-158 as we don't put all public and redeem scripts
-- inside the filter to save bandwidth.
{-# LANGUAGE BangPatterns #-}
module Ergvein.Filters.Btc.Index(
  -- * Constants for BIP158
    btcDefP
  , btcDefM
  -- * Outpoint quering context
  , HasTxIndex(..)
  , foldInputs
  -- * Filtering elements of filter
  , isBip158Indexable
  , isErgveinIndexable
    -- * SegWit address helpers
  , addressToScriptBS
  ) where

import Data.Word
import Network.Haskoin.Address
import Network.Haskoin.Block
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Data.ByteString ( ByteString )

-- | Default value for P parameter (amount of bits in golomb rice encoding).
-- Set to fixed `19` according to BIP-158.
btcDefP :: Int
btcDefP = 19

-- | Default value for M parameter (the target false positive rate).
-- Set to fixed `784931` according to BIP-158.
btcDefM :: Word64
btcDefM = 784931

-- | Creation of filters require quering of each output script for each input tx,
-- so you need provide  a way `makeBtcFilter` can query them.
class Monad m => HasTxIndex m where
  queryOutPoint :: OutPoint -> m (Maybe ByteString)

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
isBip158Indexable bs = case decodeOutputBS bs of
  Right (DataCarrier          {}) -> False
  _                               -> True
{-# INLINE isBip158Indexable #-}

-- | We index only segwit scripts, multisigs and data carrier for USDT support.
isErgveinIndexable :: ByteString -> Bool
isErgveinIndexable bs = case decodeOutputBS bs of
  Right (PayWitnessPKHash     {}) -> True
  Right (PayWitnessScriptHash {}) -> True
  Right (PayMulSig            {}) -> True
  Right (DataCarrier          {}) -> True
  _                               -> False
{-# INLINE isErgveinIndexable #-}
