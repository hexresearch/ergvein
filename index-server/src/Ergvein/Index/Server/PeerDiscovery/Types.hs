module Ergvein.Index.Server.PeerDiscovery.Types where

import Data.Time
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Servant.Client.Core.BaseUrl
import Ergvein.Index.Server.DB.Schema
import Data.Time.Clock
import Data.Text
import Data.Set (Set)

data CurrencyOutOfSyncInfo = CurrencyOutOfSyncInfo
  { outOfsyncCurrency    :: Currency
  , outOfSyncLocalHeight :: BlockHeight
  } deriving Show

data PeerValidationResult = OK
  | AlreadyKnown
  | InfoEndpointError
  | KnownPeersEndpointError
  | CurrencyOutOfSync CurrencyOutOfSyncInfo
  | CurrencyMissing Currency
  deriving Show

data PeerCandidate = PeerCandidate
  { peerCandidateUrl :: BaseUrl
  }

data Peer = Peer
  { peerId               :: DiscoveredPeerRecId
  , peerUrl              :: BaseUrl
  , peerLastValidatedAt  :: UTCTime
  , peerConnScheme       :: Scheme
  } deriving Show

data NewPeer = NewPeer
  { newPeerUrl        :: BaseUrl
  , newPeerConnScheme :: Scheme
  }

data PeerDiscoveryRequisites = PeerDiscoveryRequisites
  { descReqOwnAddress           :: !(Maybe BaseUrl)
  , descReqPredefinedPeers      :: !(Set BaseUrl)
  , descReqActualizationDelay   :: !Int
  , descReqActualizationTimeout :: !NominalDiffTime
  }