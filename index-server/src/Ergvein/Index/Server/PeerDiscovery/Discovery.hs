module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor
import Debug.Trace
import Ergvein.Index.API.Types
import Ergvein.Index.Client.V1
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.PeerDiscovery.Types
import Servant.Client.Core
import qualified Network.HTTP.Client as HC
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Monad
import qualified Data.Map.Strict as Map

instance MonadIO m => HasClientManager (ReaderT HC.Manager m) where
  getClientManager = ask
  {-# INLINE getClientManager #-}

exeToSchema :: (HasHttpManager m, HasTlsManager m, HasClientManager m) => Scheme -> ReaderT HC.Manager m a -> m a
exeToSchema s mm = do
  mgr <- case s of  
          Http ->  getHttpManager
          Https -> getTlsManager
  runReaderT mm mgr

valResult :: InfoResponse -> ServerM (Either PeerValidationResult InfoResponse)
valResult r = do
  r' <-  scanningInfo
  let rd = Map.fromList $ (\x-> (scanProgressCurrency x, scanProgressScannedHeight x)) <$> infoScanProgress r
  undefined

considerPeerCandidate :: (HasTlsManager m, HasHttpManager m, HasClientManager m) => PeerCandidate -> m PeerValidationResult
considerPeerCandidate c = do
  let schema = baseUrlScheme $ peerCandidateUrl c
  infoResult <- exeToSchema schema $ getInfoEndpoint (peerCandidateUrl c) ()
  let x = first (const Fail) infoResult
  liftIO $ traceIO $ show infoResult
  pure $ case infoResult of
            Right resp -> OK
            Left fail -> Fail