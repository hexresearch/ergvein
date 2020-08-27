module Ergvein.Index.Server.PeerDiscovery.Types where

import Data.Time
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Servant.Client.Core.BaseUrl
import Data.Time.Clock
import Data.Text
import Data.Set (Set)
import Network.Socket

data CurrencyOutOfSyncInfo = CurrencyOutOfSyncInfo
  { outOfsyncCurrency    :: Currency
  , outOfSyncLocalHeight :: BlockHeight
  } deriving Show

data PeerValidationResult = OK
  | UrlFormatError
  | AlreadyKnown
  | InfoEndpointError
  | KnownPeersEndpointError
  | CurrencyOutOfSync CurrencyOutOfSyncInfo
  | CurrencyMissing Currency
  deriving Show

data PeerCandidate = PeerCandidate
  { peerCandidateUrl :: String
  }

data Peer = Peer
  { peerUrl              :: BaseUrl
  , peerLastValidatedAt  :: UTCTime
  , peerConnScheme       :: Scheme
  } deriving Show

data Peer1 = Peer1
  { peerAddress           :: SockAddr
  , peerLastValidatedAt1  :: UTCTime
  } deriving Show

data NewPeer = NewPeer
  { newPeerUrl        :: BaseUrl
  , newPeerConnScheme :: Scheme
  }

data PeerDiscoveryRequisites = PeerDiscoveryRequisites
  { descReqOwnAddress           :: !(Maybe SockAddr)
  , descReqPredefinedPeers      :: !(Set SockAddr)
  , descReqActualizationDelay   :: !Int
  , descReqActualizationTimeout :: !NominalDiffTime
  }