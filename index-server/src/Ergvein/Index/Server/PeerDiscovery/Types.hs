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

data NewPeer = NewPeer
  { discPeerUrl :: BaseUrl
  , discPeerLastValidatedAt :: UTCTime
  , discPeerConnectionScheme :: Scheme
  }

data NNewPeer = NNewPeer
  { ndiscId :: DiscoveredPeerRecId
  , ndiscPeerUrl :: BaseUrl
  , ndiscPeerLastValidatedAt :: UTCTime
  , ndiscPeerConnectionScheme :: Scheme
  }