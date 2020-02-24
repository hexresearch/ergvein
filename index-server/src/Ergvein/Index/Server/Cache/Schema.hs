{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.Cache.Schema where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block
import Data.Flat
import System.ByteOrder
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
  { txOutCacheRecKeyPubKeyScriptHash :: PubKeyScriptHash
  } deriving (Generic, Flat)

data TxOutCacheRecItem = TxOutCacheRecItem
  { txOutCacheRecIndex  :: TxOutIndex
  , txOutCacheRecValue  :: MoneyUnit
  , txOutCacheRecTxHash :: TxHash
  } deriving (Generic, Flat)

type TxOutCacheRec = [TxOutCacheRecItem]

--TxIn

cachedTxInKey :: (PubKeyScriptHash, TxOutIndex) -> ByteString
cachedTxInKey = keyString "\0\0\0\1" . uncurry TxInCacheRecKey

data TxInCacheRecKey = TxInCacheRecKey
  { txInCacheRecKeyTxOutHash :: PubKeyScriptHash
  , txInCacheRecKeyTxOutIndex :: TxOutIndex
  } deriving (Generic, Flat)

data TxInCacheRec = TxInCacheRec
  { txInCacheRecTxHash  :: TxHash
  } deriving (Generic, Flat)

--Tx

cachedTxKey :: TxHash -> ByteString
cachedTxKey = keyString "\0\0\1\0" . TxCacheRecKey

data TxCacheRecKey = TxCacheRecKey
  { txCacheRecKeyHash         :: TxHash
  } deriving (Generic, Flat)

data TxCacheRec = TxCacheRec
  { txCacheRecHash         :: TxHash
  , txCacheRecHexView      :: TxHexView
  , txCacheRecBlockHeight  :: BlockHeight
  , txCacheRecBlockIndex   :: TxBlockIndex
  } deriving (Generic, Flat)

--BlockMeta

cachedMetaKey :: (Currency, BlockHeight) -> ByteString
cachedMetaKey (c, bh) = keyString "\0\0\1\1" $ BlockMetaCacheRecKey c $ LEHeight bh

data BlockMetaCacheRecKey = BlockMetaCacheRecKey
  { blockMetaCacheRecKeyCurrency     :: Currency
  , blockMetaCacheRecKeyBlockHeight  :: LEHeight
  } deriving (Generic, Eq, Ord, Flat, Show)

data BlockMetaCacheRec = BlockMetaCacheRec
  { blockMetaCacheRecHeaderHexView  :: BlockHeaderHexView
  , blockMetaCacheRecAddressFilterHexView :: AddressFilterHexView
  } deriving (Generic, Flat)

newtype LEHeight = LEHeight { unLEHeight :: BlockHeight }
  deriving (Generic, Eq, Ord, Show)

instance Flat LEHeight where 
  encode = encode . toBigEndian . unLEHeight
  {-# INLINE encode #-}
  decode = LEHeight . fromBigEndian <$> decode
  {-# INLINE decode #-}
  size (LEHeight a) = size $ toBigEndian a 
  {-# INLINE size #-}