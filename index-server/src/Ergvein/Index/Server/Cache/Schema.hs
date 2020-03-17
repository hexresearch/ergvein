{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.Cache.Schema where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block
import Data.Flat
import Data.ByteString (ByteString)
import System.ByteOrder
import qualified Data.ByteString as BS

data KeyPrefix = Meta | TxOut | TxIn | Tx deriving Enum


unflatExact :: (Flat b) => ByteString -> b
unflatExact s = case unflat s of
    Right k -> k
    Left e -> error $ show e ++ "value " ++  show s

keyString :: (Flat k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` flat key

unPrefixedKey key = BS.tail key
  where err = error $ "unPrefixedKey error"

parsedCacheKey :: (Flat k) => ByteString -> k
parsedCacheKey = unflatExact . unPrefixedKey

--TxOut

cachedTxOutKey :: PubKeyScriptHash -> ByteString
cachedTxOutKey = keyString TxOut . TxOutCacheRecKey

data TxOutCacheRecKey = TxOutCacheRecKey
  { txOutCacheRecKeyPubKeyScriptHash :: PubKeyScriptHash
  } deriving (Generic, Show, Eq, Ord, Flat)

data TxOutCacheRecItem = TxOutCacheRecItem
  { txOutCacheRecIndex  :: TxOutIndex
  , txOutCacheRecValue  :: MoneyUnit
  , txOutCacheRecTxHash :: TxHash
  } deriving (Generic, Show, Eq, Ord, Flat)

type TxOutCacheRec = [TxOutCacheRecItem]

--TxIn

cachedTxInKey :: (PubKeyScriptHash, TxOutIndex) -> ByteString
cachedTxInKey = keyString TxIn . uncurry TxInCacheRecKey

data TxInCacheRecKey = TxInCacheRecKey
  { txInCacheRecKeyTxOutHash :: PubKeyScriptHash
  , txInCacheRecKeyTxOutIndex :: TxOutIndex
  } deriving (Generic, Show, Eq, Ord, Flat)

data TxInCacheRec = TxInCacheRec
  { txInCacheRecTxHash  :: TxHash
  } deriving (Generic, Show, Eq, Ord, Flat)

--Tx

cachedTxKey :: TxHash -> ByteString
cachedTxKey = keyString Tx . TxCacheRecKey

data TxCacheRecKey = TxCacheRecKey
  { txCacheRecKeyHash         :: TxHash
  } deriving (Generic, Show, Eq, Ord, Flat)

data TxCacheRec = TxCacheRec
  { txCacheRecHash         :: TxHash
  , txCacheRecHexView      :: TxHexView
  , txCacheRecBlockHeight  :: BlockHeight
  , txCacheRecBlockIndex   :: TxBlockIndex
  } deriving (Generic, Show, Eq, Ord, Flat)

--BlockMeta

cachedMetaKey :: (Currency, BlockHeight) -> ByteString
cachedMetaKey (c, bh) = keyString Meta $ BlockMetaCacheRecKey c $ LEHeight bh 

data BlockMetaCacheRecKey = BlockMetaCacheRecKey
  { blockMetaCacheRecKeyCurrency     :: Currency
  , blockMetaCacheRecKeyBlockHeight  :: LEHeight
  } deriving (Generic, Show, Eq, Ord, Flat)

data BlockMetaCacheRec = BlockMetaCacheRec
  { blockMetaCacheRecHeaderHexView  :: BlockHeaderHexView
  , blockMetaCacheRecAddressFilterHexView :: AddressFilterHexView
  } deriving (Generic, Show, Eq, Ord, Flat)

newtype LEHeight = LEHeight { unLEHeight :: BlockHeight }
  deriving (Generic, Eq, Ord, Show)

instance Flat LEHeight where
  encode = encode . toBigEndian . unLEHeight
  {-# INLINE encode #-}
  decode = LEHeight . fromBigEndian <$> decode
  {-# INLINE decode #-}
  size (LEHeight a) = size $ toBigEndian a
  {-# INLINE size #-} 

