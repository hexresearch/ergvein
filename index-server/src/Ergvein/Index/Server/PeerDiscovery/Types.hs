module Ergvein.Index.Server.PeerDiscovery.Types where

import Servant.Client.Core.BaseUrl

data PeerCandidate = PeerCandidate 
    { peerCandidateUrl :: BaseUrl 
    }