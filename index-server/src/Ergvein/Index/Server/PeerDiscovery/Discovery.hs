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
import Control.Monad.Trans.Except

instance MonadIO m => HasClientManager (ReaderT HC.Manager m) where
  getClientManager = ask
  {-# INLINE getClientManager #-}

candidateInfo :: (HasHttpManager m, HasTlsManager m, HasClientManager m) => Scheme -> BaseUrl -> ExceptT PeerValidationResult m InfoResponse
candidateInfo schema baseUrl = do
  connectionManager <- lift $ 
    case schema of  
      Http  -> getHttpManager
      Https -> getTlsManager

  info <- runReaderT (getInfoEndpoint baseUrl ()) connectionManager
  except $ first (const Fail) info

candidateScanValidation :: InfoResponse -> ExceptT PeerValidationResult ServerM ()
candidateScanValidation candidateInfo = do
  localInfo <- lift $ scanningInfo
  let isCandidateScanMatchLocal = all isCurrencyScannedEnough localInfo
  except $ if isCandidateScanMatchLocal then Right () else Left Fail
  where
   isCurrencyScannedEnough candidate = 
    any (notLessThenOne (nfoScannedHeight candidate) . scanProgressScannedHeight) 
    $ candidateInfoMap Map.!? nfoCurrency candidate
   notLessThenOne local = (local <=) . succ
   candidateInfoMap = Map.fromList $ (\scanInfo -> (scanProgressCurrency scanInfo, scanInfo)) 
                                  <$> infoScanProgress candidateInfo

considerPeerCandidate :: PeerCandidate -> ExceptT PeerValidationResult ServerM ()
considerPeerCandidate candidate = do
  let candidateSchema = baseUrlScheme $ peerCandidateUrl candidate
  infoResult <- candidateInfo candidateSchema $ peerCandidateUrl candidate
  candidateScanResult <- candidateScanValidation infoResult
  pure candidateScanResult