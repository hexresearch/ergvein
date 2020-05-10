module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Control.Concurrent
import Control.Immortal
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Either.Combinators
import Data.Hashable
import Data.Time.Clock
import Ergvein.Index.API.Types
import Ergvein.Index.Client.V1
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Types
import Servant.Client.Core
import Data.Maybe
import Data.Foldable

import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as HC
import qualified Data.HashSet as Set

instance Hashable BaseUrl where
  hashWithSalt salt = hashWithSalt salt . showBaseUrl

peerInfoRequest :: BaseUrl -> ExceptT PeerValidationResult ServerM InfoResponse
peerInfoRequest baseUrl =
  ExceptT $ (const InfoConnectionError `mapLeft`) 
         <$> getInfoEndpoint baseUrl ()

peerKnownPeersRequest :: BaseUrl -> ExceptT PeerValidationResult ServerM KnownPeersResp
peerKnownPeersRequest baseUrl =
  ExceptT $ (const KnownPeersConnectionError `mapLeft`)
         <$> getKnownPeersEndpoint baseUrl (KnownPeersReq False)

peerActualScan :: InfoResponse -> ExceptT PeerValidationResult ServerM InfoResponse
peerActualScan candidateInfo = do
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

extractAddresses :: KnownPeersResp -> [NewPeer]
extractAddresses = fmap newPeer . mapMaybe parseBaseUrl . knownPeersList
  where
    newPeer url = NewPeer  url (baseUrlScheme url)


peerKnownPeers :: BaseUrl -> ExceptT PeerValidationResult ServerM [NewPeer]
peerKnownPeers baseUrl = do
  infoResult <- peerInfoRequest baseUrl
  candidateScanResult <- peerActualScan infoResult
  knownPeers <- peerKnownPeersRequest baseUrl
  pure $ extractAddresses knownPeers

considerPeerCandidate :: PeerCandidate -> ExceptT PeerValidationResult ServerM ()
considerPeerCandidate candidate = do
  let baseUrl = peerCandidateUrl candidate
  let candidateSchema = baseUrlScheme baseUrl
  candidateScanResult <- peerKnownPeers baseUrl
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
      discoveryRequisites <- getDiscoveryRequisites
      ownAddress <- peerDescOwnAddress <$> getDiscoveryRequisites
      discoveredPeers <- dbQuery getNewPeers
      let discoveredPeersSet = Set.fromList $ toList ownAddress <> (peerUrl <$> discoveredPeers)
      forM_ discoveredPeers (discoverIteration discoveredPeersSet)
      liftIO $ threadDelay $ configBlockchainScanDelay cfg
      pure ()

    discoverIteration :: Set.HashSet BaseUrl -> Peer -> ServerM ()
    discoverIteration discoveredPeersSet peer = do
      retryTimeout <- peerDescConnectionRetryTimeout <$> getDiscoveryRequisites
      currentTime <- liftIO getCurrentTime
      let fromLastSuccess = currentTime `diffUTCTime` peerLastValidatedAt peer
      if (fromLastSuccess <= retryTimeout) then void <$> runExceptT $ do
        newPeers <- peerKnownPeers $ peerUrl peer
        let uniqueNewPeers = filter (not . flip Set.member discoveredPeersSet . newPeerUrl)  newPeers
        lift $ do
          forM_ uniqueNewPeers (\x -> dbQuery $ addNewPeer x)
          dbQuery $ refreshPeerValidationTime $ peerId peer
      else
        pure ()

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