module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Control.Concurrent
import Control.Immortal
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Either.Combinators
import Data.Foldable
import Data.Hashable
import Data.Maybe
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

extractAddresses :: KnownPeersResp -> [BaseUrl]
extractAddresses = mapMaybe parseBaseUrl . knownPeersList

peerKnownPeers :: BaseUrl -> ExceptT PeerValidationResult ServerM [BaseUrl]
peerKnownPeers baseUrl = do
  infoResult <- peerInfoRequest baseUrl
  candidateScanResult <- peerActualScan infoResult
  knownPeers <- peerKnownPeersRequest baseUrl
  pure $ extractAddresses knownPeers

considerPeerCandidate :: PeerCandidate -> ExceptT PeerValidationResult ServerM ()
considerPeerCandidate candidate = do
  let baseUrl = peerCandidateUrl candidate   
  candidateScanResult <- peerKnownPeers baseUrl
  let newPeer = NewPeer baseUrl candidateSchema
      candidateSchema = baseUrlScheme baseUrl
  lift $ dbQuery $ addNewPeers [newPeer] 

peerDiscoverActualization :: ServerM Thread
peerDiscoverActualization = do
  create $ logOnException . scanIteration
  where
    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      cfg <- serverConfig
      discoveryRequisites <- getDiscoveryRequisites
      ownAddress <- peerDescOwnAddress <$> getDiscoveryRequisites
      discoveredPeers <- dbQuery $ getDiscoveredPeers False
      let discoveredPeersSet = Set.fromList $ toList ownAddress ++ (peerUrl <$> discoveredPeers)
      sequenceA_ $ discoverIteration discoveredPeersSet <$> discoveredPeers
      liftIO $ threadDelay $ configBlockchainScanDelay cfg

    discoverIteration :: Set.HashSet BaseUrl -> Peer -> ServerM ()
    discoverIteration discoveredPeersSet peer = do
      retryTimeout <- peerDescConnectionRetryTimeout <$> getDiscoveryRequisites
      currentTime <- liftIO getCurrentTime
      let fromLastSuccess = currentTime `diffUTCTime` peerLastValidatedAt peer
      if (fromLastSuccess <= retryTimeout) then void <$> runExceptT $ do
        newPeers <- peerKnownPeers $ peerUrl peer
        let uniqueNewPeers = filter (not . flip Set.member discoveredPeersSet) newPeers
        lift $ dbQuery $ do
          addNewPeers $ (\newPeerUrl -> NewPeer newPeerUrl (baseUrlScheme newPeerUrl)) <$> uniqueNewPeers
          refreshPeerValidationTime [peerId peer]
      else
        dbQuery $ deleteExpiredPeers [peerId peer]

peerIntroduce :: ServerM ()
peerIntroduce = void $ runMaybeT $ do
  ownAddress <- MaybeT $ peerDescOwnAddress <$> getDiscoveryRequisites
  lift $ do
    allPeers <- dbQuery $ getDiscoveredPeers False
    let introduceReq = IntroducePeerReq $ showBaseUrl ownAddress
    forM_ allPeers (flip getIntroducePeerEndpoint introduceReq . peerUrl)

addDefaultPeersIfNoneDiscovered :: ServerM ()
addDefaultPeersIfNoneDiscovered = do
  isNoneDiscovered <- dbQuery isNonePeersDiscovered
  when isNoneDiscovered $ do
    knownPeers <- peerDescKnownPeers <$> getDiscoveryRequisites
    dbQuery $ addNewPeers $ (\knownPeerBaseUrl -> NewPeer knownPeerBaseUrl (baseUrlScheme knownPeerBaseUrl)) <$> knownPeers