{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.Cache.Schema where

import Data.ByteString (ByteString)
import Data.Either
import Data.Flat
import Data.Serialize (Serialize) 
import Data.Text
import Data.Text.Encoding
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import System.ByteOrder
import Data.FileEmbed
import Crypto.Hash.SHA256

import qualified Data.ByteString as BS
import qualified Data.Serialize as S

data KeyPrefix = Meta | Tx | SchemaVersion deriving Enum

schemaVersion = hash $(embedFile "src/Ergvein/Index/Server/Cache/Schema.hs")

instance Serialize Text where
  put txt = S.put $ encodeUtf8 txt
  get     = decodeUtf8 <$> S.get

unflatExact :: (Flat a) => ByteString -> a
unflatExact s = case unflat s of
    Right k -> k
    Left e -> error $ show e ++ "value " ++  show s

decodeExact :: (Serialize b) => ByteString -> b
decodeExact s = case S.decode s of
    Right k -> k
    Left e -> error e

keyString :: (Serialize k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` S.encode key

unPrefixedKey key = BS.tail key
  where err = error $ "unPrefixedKey error"

parsedCacheKey :: Serialize k => ByteString -> k
parsedCacheKey = fromRight (error "ser") . S.decode . unPrefixedKey

--Tx

cachedTxKey :: TxHash -> ByteString
cachedTxKey = keyString Tx . TxCacheRecKey

data TxCacheRecKey = TxCacheRecKey
  { txCacheRecKeyHash      :: TxHash
  } deriving (Generic, Show, Eq, Ord, Serialize)

data TxCacheRec = TxCacheRec
  { txCacheRecHash         :: TxHash
  , txCacheRecHexView      :: TxHexView
  , txCacheRecUnspentOutputsCount :: Word
  } deriving (Generic, Show, Eq, Ord, Flat)

--BlockMeta

cachedMetaKey :: (Currency, BlockHeight) -> ByteString
cachedMetaKey  = keyString Meta . uncurry BlockMetaCacheRecKey

data BlockMetaCacheRecKey = BlockMetaCacheRecKey
  { blockMetaCacheRecKeyCurrency     :: Currency
  , blockMetaCacheRecKeyBlockHeight  :: BlockHeight
  } deriving (Generic, Show, Eq, Ord, Serialize)

data BlockMetaCacheRec = BlockMetaCacheRec
  { blockMetaCacheRecHeaderHashHexView  :: BlockHeaderHashHexView
  , blockMetaCacheRecAddressFilterHexView :: AddressFilterHexView
  } deriving (Generic, Show, Eq, Ord, Flat)

--BlockMeta

cachedSchemaVersionKey :: ByteString
cachedSchemaVersionKey  = keyString SchemaVersion (mempty @String)

data SchemaVersionCacheRec = Text  deriving (Generic, Show, Eq, Ord, Flat)

