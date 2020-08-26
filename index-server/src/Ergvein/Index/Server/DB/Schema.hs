{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema where

import Crypto.Hash.SHA256
import Data.ByteString (ByteString)
import Data.Either
import Data.FileEmbed
import Data.Flat
import Data.Serialize (Serialize) 
import Data.Text
import Data.Text.Encoding
import Data.Time
import Data.Word
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import System.ByteOrder

import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map

data KeyPrefix = ScannedHeight | Meta | Tx | Peer | LastBlockHash | ContentHistory | SchemaVersion deriving Enum

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
  , txRecUnspentOutputsCount :: Word32
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

data PeerAddress = V4 Word32
                 | V6 (Word32, Word32, Word32, Word32)
  deriving (Generic, Show, Eq, Ord, Flat)

data KnownPeerRecItem1 = KnownPeerRecItem1
  { knownPeerRecIP               :: PeerAddress
  , knownPeerRecPort             :: Word16
  , knownPeerRecLastValidatedAt1 :: Text
  } deriving (Generic, Show, Eq, Ord, Flat)

--lastScannedBlockHeaderHash

lastScannedBlockHeaderHashRecKey :: Currency -> ByteString
lastScannedBlockHeaderHashRecKey  = keyString LastBlockHash . LastScannedBlockHeaderHashRecKey

data LastScannedBlockHeaderHashRecKey = LastScannedBlockHeaderHashRecKey
  { lastScannedBlockHeaderHashRecKeyCurrency :: Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data LastScannedBlockHeaderHashRec = LastScannedBlockHeaderHashRec
  { lastScannedBlockHeaderHashRecHash :: BlockHeaderHashHexView
  } deriving (Generic, Show, Eq, Ord, Flat)

--ScannedContentHistory

contentHistoryRecKey :: Currency -> ByteString
contentHistoryRecKey  = keyString ContentHistory . ContentHistoryRecKey

data ContentHistoryRecKey = ContentHistoryRecKey
  { contentHistoryRecKeyCurrency :: Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data ContentHistoryRec = ContentHistoryRec
  { contentHistoryRecItems :: Seq.Seq ContentHistoryRecItem
  } deriving (Generic, Show, Eq, Ord, Flat)

data ContentHistoryRecItem = ContentHistoryRecItem
  { contentHistoryRecItemSpentTxOuts  :: Map.Map TxHash Word32
  , contentHistoryRecItemAddedTxsHash :: [TxHash]
  } deriving (Generic, Show, Eq, Ord, Flat)

contentHistorySize :: Int
contentHistorySize = 64

--SchemaVersion

schemaVersionRecKey :: ByteString
schemaVersionRecKey  = keyString SchemaVersion $ mempty @String

data SchemaVersionRec = Text  deriving (Generic, Show, Eq, Ord, Flat)
