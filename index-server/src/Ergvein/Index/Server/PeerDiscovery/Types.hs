module Ergvein.Index.Server.PeerDiscovery.Types where

import Servant.Client.Core.BaseUrl
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block

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