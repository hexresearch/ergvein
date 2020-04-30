module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Either.Combinators
import Ergvein.Index.API.Types
import Ergvein.Index.Client.V1
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Types
import Servant.Client.Core

import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as HC

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
  except $ mapLeft (const PeerConnectionError) info

candidateScanValidation :: InfoResponse -> ExceptT PeerValidationResult ServerM ()
candidateScanValidation candidateInfo = do
  localInfo <- lift $ scanningInfo
  let isCandidateScanMatchLocal = void <$> sequence $ currencyScanValidation <$> localInfo
  except $ isCandidateScanMatchLocal
  where
    notLessThenOne local = (local <=) . succ
    currencyScanValidation localInfo = do
      let localCurrency = nfoCurrency localInfo
          localScannedHeight =  nfoScannedHeight localInfo
      candidateNfo <- maybeToRight (CurrencyMissing localCurrency) $ candidateInfoMap Map.!? localCurrency
      if notLessThenOne localScannedHeight (scanProgressScannedHeight candidateNfo) then
        Right ()
      else 
        Left $ CurrencyOutOfSync $ CurrencyOutOfSyncInfo localCurrency localScannedHeight

    candidateInfoMap = Map.fromList $ (\scanInfo -> (scanProgressCurrency scanInfo, scanInfo)) 
                                   <$> infoScanProgress candidateInfo

considerPeerCandidate :: PeerCandidate -> ExceptT PeerValidationResult ServerM ()
considerPeerCandidate candidate = do
  let baseUrl = peerCandidateUrl candidate
  let candidateSchema = baseUrlScheme baseUrl
  infoResult <- candidateInfo candidateSchema baseUrl
  candidateScanResult <- candidateScanValidation infoResult
  pure candidateScanResult

