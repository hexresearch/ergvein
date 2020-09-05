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

data Peer = Peer
  { peerAddress           :: SockAddr
  , peerLastValidatedAt1  :: UTCTime
  } deriving Show

data NewPeer = NewPeer
  { newPeerAddress :: SockAddr
  }

data PeerDiscoveryRequisites = PeerDiscoveryRequisites
  { descReqOwnAddress           :: !(Maybe SockAddr)
  , descReqPredefinedPeers      :: !(Set SockAddr)
  , descReqActualizationDelay   :: !Int
  , descReqActualizationTimeout :: !NominalDiffTime
  }