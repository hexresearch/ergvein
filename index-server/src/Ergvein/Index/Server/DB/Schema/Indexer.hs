{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Indexer
  (
    KnownPeerRecItem(..)
  , KnownPeersRec(..)
  , LastScannedBlockHeaderHashRec(..)
  , RollbackKey(..)
  , RollbackRecItem(..)
  , RollbackSequence(..)
  ) where

import Conversion
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Builder
import GHC.Generics
import Data.Serialize (Serialize)
import Data.Text (Text,pack,unpack)

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Index.Server.PeerDiscovery.Types (PeerAddr(..), PeerIP(..))
import Ergvein.Index.Protocol.Types (Address(..),IPType(..))
import qualified Ergvein.Index.Server.PeerDiscovery.Types as DiscoveryTypes

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Sequence           as Seq

-- ===========================================================================
--           Known peers
-- ===========================================================================

data KnownPeersRec = KnownPeersRec {
    unKnownPeersRec :: [KnownPeerRecItem]
  } deriving (Generic, Show, Eq, Ord)

data KnownPeerRecItem = KnownPeerRecItem
  { knownPeerRecAddr             :: DiscoveryTypes.PeerAddr
  , knownPeerRecLastValidatedAt  :: Text
  } deriving (Generic, Show, Eq, Ord)

-- ===========================================================================
--           Last scanned block header
-- ===========================================================================

data LastScannedBlockHeaderHashRecKey = LastScannedBlockHeaderHashRecKey
  { lastScannedBlockHeaderHashRecKeyCurrency :: !Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data LastScannedBlockHeaderHashRec = LastScannedBlockHeaderHashRec
  { lastScannedBlockHeaderHashRecHash :: !ShortByteString
  } deriving (Generic, Show, Eq, Ord)

-- ===========================================================================
--           Rollback info
-- ===========================================================================

data RollbackKey = RollbackKey { unRollbackKey :: !Currency }
  deriving (Generic, Show, Eq, Ord, Serialize)

data RollbackRecItem = RollbackRecItem
  { rollbackItemAdded     :: [TxHash]
  , rollbackPrevBlockHash :: !BlockHash
  , rollbackPrevHeight    :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord)

data RollbackSequence = RollbackSequence { unRollbackSequence :: Seq.Seq RollbackRecItem}
  deriving (Generic, Show, Eq, Ord)

-- ===========================================================================
--           Conversion instances
-- ===========================================================================

instance Conversion DiscoveryTypes.Peer KnownPeerRecItem where
  convert DiscoveryTypes.Peer{..} = let
    validatedAt = pack $ show $ peerLastValidatedAt
    in KnownPeerRecItem
      { knownPeerRecAddr = convert peerAddress
      , knownPeerRecLastValidatedAt = validatedAt
      }

instance Conversion KnownPeerRecItem DiscoveryTypes.Peer where
  convert KnownPeerRecItem {..} = DiscoveryTypes.Peer
    { peerAddress = convert knownPeerRecAddr
    , peerLastValidatedAt = read $ unpack knownPeerRecLastValidatedAt
    }

instance Conversion KnownPeerRecItem Address where
  convert KnownPeerRecItem {..} = let
    in case peerAddrIP knownPeerRecAddr of
      V4 ip -> Address
        { addressType    = IPV4
        , addressPort    = peerAddrPort knownPeerRecAddr
        , addressAddress = BL.toStrict $ toLazyByteString $ word32BE ip
        }
      V6 (a,b,c,d) -> Address
        { addressType    = IPV6
        , addressPort    = peerAddrPort knownPeerRecAddr
        , addressAddress = BL.toStrict $ toLazyByteString $ word32BE a <> word32BE b <> word32BE c <> word32BE d
        }
