{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.Cache.Schema where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Data.Flat
import Data.ByteString


unflatExact' :: (Flat b) => ByteString -> b
unflatExact' s = case unflat s of
    Right k -> k
    Left e -> error $ show e ++ "value " ++  show s

keyString :: (Flat k) => ByteString -> k -> ByteString
keyString keyPrefix key = keyPrefix <> flat key

unPrefixedKey key = Data.ByteString.drop 4 key
  where err = error $ "unPrefixedKey error"

parsedCacheKey :: (Flat k) => ByteString -> k
parsedCacheKey = unflatExact' . unPrefixedKey

--TxOut

cachedTxOutKey :: PubKeyScriptHash -> ByteString
cachedTxOutKey = keyString "\0\0\0\0" . TxOutCacheRecKey

data TxOutCacheRecKey = TxOutCacheRecKey
  { txOutCacheRecKey'pubKeyScriptHash :: PubKeyScriptHash
  } deriving (Generic, Flat)

data TxOutCacheRecItem = TxOutCacheRecItem
  { txOutCacheRec'index  :: TxOutIndex
  , txOutCacheRec'value  :: MoneyUnit
  , txOutCacheRec'txHash :: TxHash
  } deriving (Generic, Flat)

type TxOutCacheRec = [TxOutCacheRecItem]

--TxIn

cachedTxInKey :: (PubKeyScriptHash, TxOutIndex) -> ByteString
cachedTxInKey = keyString "\0\0\0\1" . uncurry TxInCacheRecKey

data TxInCacheRecKey = TxInCacheRecKey
  { txInCacheRecKey'txOutHash :: PubKeyScriptHash
  , txInCacheRecKey'txOutIndex :: TxOutIndex
  } deriving (Generic, Flat)

data TxInCacheRec = TxInCacheRec
  { txInCacheRec'txHash  :: TxHash
  } deriving (Generic, Flat)

--Tx

cachedTxKey :: TxHash -> ByteString
cachedTxKey = keyString "\0\0\1\0" . TxCacheRecKey

data TxCacheRecKey = TxCacheRecKey
  { txCacheRecKey'hash         :: TxHash
  } deriving (Generic, Flat)

data TxCacheRec = TxCacheRec
  { txCacheRec'hash         :: TxHash
  , txCacheRec'blockHeight  :: BlockHeight
  , txCacheRec'blockIndex   :: TxBlockIndex
  } deriving (Generic, Flat)

--BlockMeta

cachedMetaKey :: (Currency, BlockHeight) -> ByteString
cachedMetaKey = keyString "\0\0\1\1" . uncurry BlockMetaCacheRecKey

data BlockMetaCacheRecKey = BlockMetaCacheRecKey
  { blockMetaCacheRecKey'currency     :: Currency
  , blockMetaCacheRecKey'blockHeight  :: BlockHeight
  } deriving (Generic, Eq, Ord, Flat)

data BlockMetaCacheRec = BlockMetaCacheRec
  { blockMetaCacheRec'headerHexView  :: TxHash
  } deriving (Generic, Flat)