module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Client.V1
import Debug.Trace
import Control.Monad.IO.Class



considerPeerCandidate :: HasClientManager m => PeerCandidate -> m PeerValidationResult
considerPeerCandidate c = do
  infoResult <- getInfoEndpoint (peerCandidateUrl c) ()
  liftIO $ traceIO $ show infoResult
  pure $ case infoResult of
            Right resp -> OK
            Left fail -> Fail