module Ergvein.Index.Server.PeerDiscovery.Types where

import Servant.Client.Core.BaseUrl

data PeerValidationResult = OK | Fail deriving Show

data PeerCandidate = PeerCandidate 
    { peerCandidateUrl :: BaseUrl 
    }