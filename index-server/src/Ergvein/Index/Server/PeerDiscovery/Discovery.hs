module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Client.V1

data PeerValidationResult = OK | Fail

considerPeerCandidate ::  PeerCandidate -> IO PeerValidationResult
considerPeerCandidate c = do
  --mgr <- HC.newManager HC.defaultManagerSettings
  --let cenv = mkClientEnv mgr $ peerCandidateUrl c
  --res <- liftIO $ flip runClientM cenv $ indexGetHeight apiV1 req 
  pure OK