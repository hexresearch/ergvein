{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.Cache.Schema where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block
import Data.Flat
import Data.Either
import Data.ByteString (ByteString)
import System.ByteOrder
import qualified Data.Serialize as Ser
import qualified Data.ByteString as BS
import Data.Text
import Data.Text.Encoding

data KeyPrefix = Meta | TxOut | TxIn | Tx deriving Enum

instance Ser.Serialize Text where
  put txt = Ser.put $ encodeUtf8 txt
  get     = fmap decodeUtf8 Ser.get

unflatExact :: (Flat b) => ByteString -> b
unflatExact s = case unflat s of
    Right k -> k
    Left e -> error $ show e ++ "value " ++  show s

decodeExact :: (Ser.Serialize b) => ByteString -> b
decodeExact s = case Ser.decode s of
    Right k -> k
    Left e -> error $ show e ++ "value " ++  show s

keyString :: (Ser.Serialize k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` Ser.encode key

unPrefixedKey key = BS.tail key
  where err = error $ "unPrefixedKey error"

parsedCacheKey :: Ser.Serialize k => ByteString -> k
parsedCacheKey = fromRight (error "ser") . Ser.decode . unPrefixedKey

--TxOut

cachedTxOutKey :: PubKeyScriptHash -> ByteString
cachedTxOutKey = keyString TxOut . TxOutCacheRecKey

data TxOutCacheRecKey = TxOutCacheRecKey
  { txOutCacheRecKeyPubKeyScriptHash :: PubKeyScriptHash
  } deriving (Generic, Show, Eq, Ord, Flat, Ser.Serialize)

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
  } deriving (Generic, Show, Eq, Ord, Flat, Ser.Serialize)

data TxInCacheRec = TxInCacheRec
  { txInCacheRecTxHash  :: TxHash
  } deriving (Generic, Show, Eq, Ord, Flat)

--Tx

cachedTxKey :: TxHash -> ByteString
cachedTxKey = keyString Tx . TxCacheRecKey

data TxCacheRecKey = TxCacheRecKey
  { txCacheRecKeyHash         :: TxHash
  } deriving (Generic, Show, Eq, Ord, Flat, Ser.Serialize)

data TxCacheRec = TxCacheRec
  { txCacheRecHash         :: TxHash
  , txCacheRecHexView      :: TxHexView
  , txCacheRecBlockHeight  :: BlockHeight
  , txCacheRecBlockIndex   :: TxBlockIndex
  } deriving (Generic, Show, Eq, Ord, Flat)

--BlockMeta

cachedMetaKey :: (Currency, BlockHeight) -> ByteString
cachedMetaKey  = keyString Meta . uncurry BlockMetaCacheRecKey

data BlockMetaCacheRecKey = BlockMetaCacheRecKey
  { blockMetaCacheRecKeyCurrency     :: Currency
  , blockMetaCacheRecKeyBlockHeight  :: BlockHeight
  } deriving (Generic, Show, Eq, Ord, Flat, Ser.Serialize)

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

newtype LECurrency = LECurrency { unLECurrency :: Currency }
  deriving (Generic, Eq, Ord, Show)

instance Flat LECurrency where
  encode = encode . toBigEndian @Word  . fromIntegral . fromEnum . unLECurrency
  {-# INLINE encode #-}
  decode = LECurrency . toEnum . fromIntegral @Word . fromBigEndian <$> decode
  {-# INLINE decode #-}
  size (LECurrency a) = size $ toBigEndian @Word $ fromIntegral $ fromEnum a
  {-# INLINE size #-} 

