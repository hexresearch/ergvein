{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Indexer
  (
    KnownPeerRecItem(..)
  , LastScannedBlockHeaderHashRec(..)
  , ContentHistoryRec(..)
  , ContentHistoryRecItem(..)
  , knownPeersRecKey
  , lastScannedBlockHeaderHashRecKey
  , contentHistoryRecKey
  , contentHistorySize
  , schemaVersionRecKey
  , schemaVersion
  ) where

import Crypto.Hash.SHA256
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Flat
import Data.Serialize (Serialize)
import Data.Text
import Data.Word

import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map

data KeyPrefix = Peer | LastBlockHash | ContentHistory | SchemaVersion deriving Enum

schemaVersion :: ByteString
schemaVersion = hash $(embedFile "src/Ergvein/Index/Server/DB/Schema/Indexer.hs")

keyString :: (Serialize k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` S.encode key

--PeerDiscovery

knownPeersRecKey :: ByteString
knownPeersRecKey  = keyString Peer $ mempty @String

data KnownPeersRec = KnownPeersRec [KnownPeerRecItem] deriving (Generic, Show, Eq, Ord, Flat)

data KnownPeerRecItem = KnownPeerRecItem
  { knownPeerRecUrl             :: Text
  , knownPeerRecIsSecureConn    :: Bool
  , knownPeerRecLastValidatedAt :: Text
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
