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

--TxOut

cachedTxOutKey :: TxOutCacheRecKey -> ByteString
cachedTxOutKey = keyString "\0\0\0\1"

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

cachedTxInKey :: TxInCacheRecKey -> ByteString
cachedTxInKey = keyString "\0\0\1\0"

data TxInCacheRecKey = TxInCacheRecKey
  { txInCacheRecKey'txOutHash :: PubKeyScriptHash
  , txInCacheRecKey'txOutIndex :: TxOutIndex
  } deriving (Generic, Flat)

data TxInCacheRec = TxInCacheRec
  { txInCacheRec'txHash  :: TxHash
  } deriving (Generic, Flat)

--Tx

cachedTxKey :: TxCacheRecKey -> ByteString
cachedTxKey = keyString "\0\0\1\1"

data TxCacheRecKey = TxCacheRecKey
  { txCacheRecKey'hash         :: TxHash
  } deriving (Generic, Flat)

data TxCacheRec = TxCacheRec
  { txCacheRec'hash         :: TxHash
  , txCacheRec'blockHeight  :: BlockHeight
  , txCacheRec'blockIndex   :: TxBlockIndex
  } deriving (Generic, Flat)

--BlockMeta

cachedMetaKeyPrefix = "\0\1\0\0"

cachedMetaKey :: (Currency, BlockHeight) -> ByteString
cachedMetaKey = keyString cachedMetaKeyPrefix . uncurry BlockMetaCacheRecKey

parsedCachedMetaKey :: ByteString -> BlockMetaCacheRecKey
parsedCachedMetaKey = unflatExact' . unPrefixedKey

data BlockMetaCacheRecKey = BlockMetaCacheRecKey
  { blockMetaCacheRecKey'currency     :: Currency
  , blockMetaCacheRecKey'blockHeight  :: BlockHeight
  } deriving (Generic, Eq, Ord, Flat)

data BlockMetaCacheRec = BlockMetaCacheRec
  { blockMetaCacheRec'headerHexView  :: TxHash
  } deriving (Generic, Flat)