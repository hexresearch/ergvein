{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Indexer
  (
    KnownPeerRecItem(..)
  , KnownPeersRec(..)
  , LastScannedBlockHeaderHashRec(..)
  , RollbackKey(..)
  , RollbackRecItem(..)
  , RollbackSequence(..)
  , rollbackKey
  , knownPeersRecKey
  , lastScannedBlockHeaderHashRecKey
  , schemaVersionRecKey
  , schemaVersion
  ) where

import Crypto.Hash.SHA256
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.FileEmbed
import GHC.Generics
import Data.Serialize (Serialize)
import Data.Text
import Data.Word

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import qualified Ergvein.Index.Server.PeerDiscovery.Types as DiscoveryTypes

import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map

data KeyPrefix = Peer | LastBlockHash | Rollback | SchemaVersion deriving Enum

schemaVersion :: ByteString
schemaVersion = hash $(embedFile "src/Ergvein/Index/Server/DB/Schema/Indexer.hs")

keyString :: (Serialize k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` S.encode key

--PeerDiscovery

knownPeersRecKey :: ByteString
knownPeersRecKey  = keyString Peer $ mempty @String

data KnownPeersRec = KnownPeersRec {
    unKnownPeersRec :: [KnownPeerRecItem]
  } deriving (Generic, Show, Eq, Ord)

data KnownPeerRecItem = KnownPeerRecItem
  { knownPeerRecAddr             :: DiscoveryTypes.PeerAddr
  , knownPeerRecLastValidatedAt  :: Text
  } deriving (Generic, Show, Eq, Ord)

--lastScannedBlockHeaderHash

lastScannedBlockHeaderHashRecKey :: Currency -> ByteString
lastScannedBlockHeaderHashRecKey  = keyString LastBlockHash . LastScannedBlockHeaderHashRecKey

data LastScannedBlockHeaderHashRecKey = LastScannedBlockHeaderHashRecKey
  { lastScannedBlockHeaderHashRecKeyCurrency :: !Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data LastScannedBlockHeaderHashRec = LastScannedBlockHeaderHashRec
  { lastScannedBlockHeaderHashRecHash :: !ShortByteString
  } deriving (Generic, Show, Eq, Ord)

-- Rollback

rollbackKey :: Currency -> ByteString
rollbackKey = keyString Rollback . RollbackKey

data RollbackKey = RollbackKey { unRollbackKey :: !Currency }
  deriving (Generic, Show, Eq, Ord, Serialize)

data RollbackRecItem = RollbackRecItem
  { rollbackItemAdded     :: [TxHash]
  , rollbackItemSpendings :: Map.Map TxHash Word32
  , rollbackPrevBlockHash :: !BlockHash
  , rollbackPrevHeight    :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord)

data RollbackSequence = RollbackSequence { unRollbackSequence :: Seq.Seq RollbackRecItem}
  deriving (Generic, Show, Eq, Ord)

--SchemaVersion

schemaVersionRecKey :: ByteString
schemaVersionRecKey  = keyString SchemaVersion $ mempty @String

data SchemaVersionRec = Text  deriving (Generic, Show, Eq, Ord)
