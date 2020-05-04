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
import Ergvein.Index.Server.DB.Queries
import Control.Concurrent
import Control.Immortal
import Data.Foldable
import Data.Time.Clock
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.Config

import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as HC

instance MonadIO m => HasClientManager (ReaderT HC.Manager m) where
  getClientManager = ask
  {-# INLINE getClientManager #-}

peerInfo :: (HasHttpManager m, HasTlsManager m, HasClientManager m) => Scheme -> BaseUrl -> ExceptT PeerValidationResult m InfoResponse
peerInfo schema baseUrl = do
  connectionManager <- lift $ 
    case schema of  
      Http  -> getHttpManager
      Https -> getTlsManager

  info <- runReaderT (getInfoEndpoint baseUrl ()) connectionManager
  except $ const PeerConnectionError `mapLeft` info

peerScanValidation :: InfoResponse -> ExceptT PeerValidationResult ServerM InfoResponse
peerScanValidation candidateInfo = do
  localInfo <- lift $ scanningInfo
  let isCandidateScanMatchLocal = sequence $ currencyScanValidation <$> localInfo
  except $ candidateInfo <$ isCandidateScanMatchLocal
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
  infoResult <- peerInfo candidateSchema baseUrl
  candidateScanResult <- peerScanValidation infoResult
  let x = NewPeer baseUrl candidateSchema
  lift $ dbQuery $ upsertNewPeer x 
  pure ()

peerDiscoverActualization :: ServerM Thread
peerDiscoverActualization = do
  create $ logOnException . scanIteration
  where
    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      cfg <- serverConfig
      allPeers <- dbQuery getNewPeers
      forM_ allPeers discoverIteration
      liftIO $ threadDelay $ configBlockchainScanDelay cfg
      pure ()

    discoverIteration :: Peer -> ServerM ()
    discoverIteration peer = void <$> runExceptT $ do
        let baseUrl = peerUrl peer
        let candidateSchema = baseUrlScheme baseUrl
        infoResult <- peerInfo candidateSchema baseUrl
        candidateScanResult <- peerScanValidation infoResult
        let x = NewPeer baseUrl candidateSchema
        lift $ dbQuery $ upsertNewPeer x 
