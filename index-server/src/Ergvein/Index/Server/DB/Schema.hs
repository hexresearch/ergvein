{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema where

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
import Data.Time

import qualified Data.ByteString as BS
import qualified Data.Serialize as S

data KeyPrefix = ScannedHeight | Meta | Tx | Peer | SchemaVersion deriving Enum

schemaVersion = hash $(embedFile "src/Ergvein/Index/Server/DB/Schema.hs")

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

parsedKey :: Serialize k => ByteString -> k
parsedKey = fromRight (error "ser") . S.decode . unPrefixedKey

--ScannedHeight

scannedHeightTxKey :: Currency -> ByteString
scannedHeightTxKey = keyString ScannedHeight . ScannedHeightRecKey

data ScannedHeightRecKey = ScannedHeightRecKey
  { scannedHeightRecKey      :: Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data ScannedHeightRec = ScannedHeightRec
  { scannedHeightRecHeight   :: BlockHeight
  } deriving (Generic, Show, Eq, Ord, Flat)

--Tx

txRecKey:: TxHash -> ByteString
txRecKey = keyString Tx . TxRecKey

data TxRecKey = TxRecKey
  { txRecKeyHash      :: TxHash
  } deriving (Generic, Show, Eq, Ord, Serialize)

data TxRec = TxRec
  { txRecHash         :: TxHash
  , txRecHexView      :: TxHexView
  , txRecUnspentOutputsCount :: Word
  } deriving (Generic, Show, Eq, Ord, Flat)

--BlockMeta

metaRecKey :: (Currency, BlockHeight) -> ByteString
metaRecKey = keyString Meta . uncurry BlockMetaRecKey

data BlockMetaRecKey = BlockMetaRecKey
  { blockMetaRecKeyCurrency     :: Currency
  , blockMetaRecKeyBlockHeight  :: BlockHeight
  } deriving (Generic, Show, Eq, Ord, Serialize)

data BlockMetaRec = BlockMetaRec
  { blockMetaRecHeaderHashHexView  :: BlockHeaderHashHexView
  , blockMetaRecAddressFilterHexView :: AddressFilterHexView
  } deriving (Generic, Show, Eq, Ord, Flat)

--PeerDiscovery

knownPeersRecKey :: ByteString
knownPeersRecKey  = keyString Peer $ mempty @String

data KnownPeersRec = KnownPeersRec [KnownPeerRecItem] deriving (Generic, Show, Eq, Ord, Flat)

data KnownPeerRecItem = KnownPeerRecItem
  { knownPeerRecUrl             :: Text
  , knownPeerRecIsSecureConn    :: Bool
  , knownPeerRecLastValidatedAt :: Text
  } deriving (Generic, Show, Eq, Ord, Flat)

schemaVersionRecKey :: ByteString
schemaVersionRecKey  = keyString SchemaVersion $ mempty @String

data SchemaVersionRec = Text  deriving (Generic, Show, Eq, Ord, Flat)
