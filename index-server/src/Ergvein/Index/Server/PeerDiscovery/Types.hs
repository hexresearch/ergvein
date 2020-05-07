module Ergvein.Index.Server.PeerDiscovery.Types where

import Data.Time
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Servant.Client.Core.BaseUrl
import Ergvein.Index.Server.DB.Schema

data CurrencyOutOfSyncInfo = CurrencyOutOfSyncInfo
  { outOfsyncCurrency :: Currency
  , outOfSyncLocalHeight :: BlockHeight
  } deriving Show

data PeerValidationResult = OK 
  | PeerConnectionError 
  | CurrencyOutOfSync CurrencyOutOfSyncInfo
  | CurrencyMissing Currency
  deriving Show

data PeerCandidate = PeerCandidate
  { peerCandidateUrl :: BaseUrl
  }

data Peer = Peer
  { peerId :: DiscoveredPeerRecId
  , peerUrl :: BaseUrl
  , peerLastValidatedAt :: UTCTime
  , peerConnectionScheme :: Scheme
  }

data NewPeer = NewPeer
  { newPeerUrl :: BaseUrl
  , newPeerConnectionScheme :: Scheme
  }

data PeerDiscoveryRequisites = PeerDiscoveryRequisites
  { peerDescOwnAddress :: !BaseUrl
  , peerDescKnownPeers :: ![BaseUrl]
  }