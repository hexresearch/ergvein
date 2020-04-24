module Ergvein.Index.Server.PeerDiscovery.Discovery where
import Control.Monad.Reader
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Client.V1
import Debug.Trace
import Control.Monad.IO.Class
import Servant.Client.Core
import qualified Network.HTTP.Client as HC
import Ergvein.Index.Server.Environment
import Ergvein.Index.API.Types

instance MonadIO m => HasClientManager (ReaderT HC.Manager m) where
  getClientManager = ask
  {-# INLINE getClientManager #-}

--exeToSchema :: (HasHttpManager m, HasTlsManager m, HasClientManager m1) => Scheme ->  m1 a -> m a
exeToSchema s mm = do
  mgr <- case s of  
          Http ->  getHttpManager
          Https -> getTlsManager
  runReaderT mm mgr

considerPeerCandidate :: (HasTlsManager m, HasHttpManager m, HasClientManager m) => PeerCandidate -> m PeerValidationResult
considerPeerCandidate c = do
  let x = baseUrlScheme $ peerCandidateUrl c
  infoResult <- getInfoEndpoint (peerCandidateUrl c) ()
  mgr <- case x of  
          Http ->  getHttpManager
          Https -> getTlsManager
  infoResult <- runReaderT (getInfoEndpoint (peerCandidateUrl c) ()) mgr
  liftIO $ traceIO $ show infoResult
  pure $ case infoResult of
            Right resp -> OK
            Left fail -> Fail