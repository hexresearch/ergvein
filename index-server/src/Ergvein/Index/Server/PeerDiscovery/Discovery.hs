module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Control.Concurrent
import Control.Immortal
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Either.Combinators
import Data.Foldable
import Data.Hashable
import Data.Time.Clock
import Ergvein.Index.API.Types
import Ergvein.Index.Client.V1
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Types
import Control.Monad.Trans.Maybe
import Servant.Client.Core
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Monad
import Data.Proxy

import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as HC
import qualified Data.HashSet as Set

peerConnectionValidation ::  BaseUrl -> ExceptT PeerValidationResult ServerM InfoResponse
peerConnectionValidation baseUrl =  ExceptT $ (const PeerConnectionError `mapLeft`) <$> getInfoEndpoint baseUrl ()

peerScanValidation :: InfoResponse -> ExceptT PeerValidationResult ServerM InfoResponse
peerScanValidation candidateInfo = do
  localInfo <- lift $ scanningInfo
  let isCandidateScanMatchLocal = sequence $ currencyScanValidation <$> localInfo
  except $ candidateInfo <$ isCandidateScanMatchLocal
  where
    notLessThenOne local = (local <=) . succ

    currencyScanValidation :: ScanProgressInfo -> Either PeerValidationResult ()
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

peerValidation :: Scheme -> BaseUrl -> ExceptT PeerValidationResult ServerM InfoResponse
peerValidation scheme baseUrl = do
  infoResult <- peerConnectionValidation baseUrl
  candidateScanResult <- peerScanValidation infoResult
  pure candidateScanResult

considerPeerCandidate :: PeerCandidate -> ExceptT PeerValidationResult ServerM ()
considerPeerCandidate candidate = do
  let baseUrl = peerCandidateUrl candidate
  let candidateSchema = baseUrlScheme baseUrl
  candidateScanResult <- peerValidation candidateSchema baseUrl
  let x = NewPeer baseUrl candidateSchema
  lift $ dbQuery $ addNewPeer x 
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
    discoverIteration peer = do
      let url = peerUrl peer
      currentTime <- liftIO getCurrentTime
      let z = currentTime `diffUTCTime` peerLastValidatedAt peer
      let s = if (z >= 86400) then Just Peer else Nothing
      rr <- runExceptT $ except $ Right  peer
      r <- ((\x -> rightToMaybe <$> (runExceptT $ peerValidation (baseUrlScheme $ peerUrl x ) (peerUrl x))))  =<< (pure peer)
      dbQuery $ refreshPeerValidationTime $ peerId peer

instance Hashable BaseUrl where
  hashWithSalt salt = hashWithSalt salt . showBaseUrl

introduceSelf :: BaseUrl -> ServerM ()
introduceSelf toPeer = void $ runMaybeT $ do
  ownAddress <- MaybeT $ peerDescOwnAddress <$> getDiscoveryRequisites
  lift $ getIntroducePeerEndpoint toPeer $ IntroducePeerReq $ showBaseUrl ownAddress

peerIntroduce :: ServerM ()
peerIntroduce = do
  allPeers <- dbQuery getNewPeers
  forM_ allPeers (introduceSelf . peerUrl)

addDefaultPeersIfNoneDiscovered :: ServerM ()
addDefaultPeersIfNoneDiscovered = do
  isNoneDiscovered <- dbQuery isNonePeersDiscovered
  when isNoneDiscovered $ do
    knownPeers <- peerDescKnownPeers <$> getDiscoveryRequisites
    forM_ knownPeers (\knownPeerBaseUrl -> do
      let newPeer = NewPeer knownPeerBaseUrl (baseUrlScheme knownPeerBaseUrl)
      dbQuery $ addNewPeer newPeer )