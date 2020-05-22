module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Control.Concurrent
import Control.Immortal
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Either.Combinators
import Data.Foldable
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Time.Clock
import Servant.Client.Core

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
import Ergvein.Index.Server.Utils
import Ergvein.Index.Server.Cache.Queries 

import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as HC
import qualified Data.HashSet as Set

knownPeers :: Bool -> ServerM [Peer]
knownPeers onlySecured = do
  actualizationDelay <- (/1000000) . fromIntegral . descReqActualizationDelay <$> getDiscoveryRequisites
  dbQuery $ getDiscoveredFilteredPeers onlySecured actualizationDelay

peerKnownPeers :: BaseUrl -> ExceptT PeerValidationResult ServerM [BaseUrl]
peerKnownPeers baseUrl = do
  infoResult <- peerInfoRequest baseUrl
  candidateScanResult <- peerActualScan infoResult
  knownPeers <- peerKnownPeersRequest baseUrl
  pure $ extractAddresses knownPeers
  where
    extractAddresses :: KnownPeersResp -> [BaseUrl]
    extractAddresses = mapMaybe parseBaseUrl . knownPeersList

considerPeerCandidate :: PeerCandidate -> ExceptT PeerValidationResult ServerM ()
considerPeerCandidate candidate = do
  let baseUrl = peerCandidateUrl candidate
  knownPeers <- lift $ dbQuery getDiscoveredPeers
  knowPeersSet <- lift $ knownPeersSet knownPeers
  if not $ Set.member baseUrl knowPeersSet then do
    _ <- peerKnownPeers baseUrl
    let newPeer = NewPeer baseUrl $ baseUrlScheme baseUrl
    lift $ dbQuery $ addNewPeers [newPeer]
    lift $ refreshKnownPeersCache
  else
    ExceptT $ pure $ Left AlreadyKnown

knownPeersSet :: [Peer] -> ServerM (Set.HashSet BaseUrl)
knownPeersSet discoveredPeers = do
  ownAddress <- descReqOwnAddress <$> getDiscoveryRequisites
  pure $ Set.fromList $ (toList ownAddress) ++ (peerUrl <$> discoveredPeers)

knownPeersActualization :: ServerM Thread
knownPeersActualization = do
  create $ logOnException . scanIteration
  where
    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      cfg <- serverConfig
      requisites <- getDiscoveryRequisites
      currentTime <- liftIO getCurrentTime
      knownPeers <- dbQuery getDiscoveredPeers

      let (outdatedPeers, peersToFetchFrom) = 
            partition (isOutdated (descReqActualizationTimeout requisites) currentTime) knownPeers
      
      knowPeersSet <- knownPeersSet peersToFetchFrom
      (peersToRefresh, fetchedPeers) <- mconcat <$> mapM peersKnownTo peersToFetchFrom

      let uniqueNotDiscoveredFetchedPeers = filter (not . (`Set.member` knowPeersSet)) $ uniqueElements fetchedPeers

      dbQuery $ do
        deleteExpiredPeers $ peerId <$> outdatedPeers
        refreshPeerValidationTime $ peerId <$> peersToRefresh
        addNewPeers $ newPeer <$> uniqueNotDiscoveredFetchedPeers
      
      refreshKnownPeersCache

      liftIO $ threadDelay $ descReqActualizationDelay requisites

    isOutdated :: NominalDiffTime -> UTCTime -> Peer -> Bool
    isOutdated retryTimeout currentTime peer = let
      fromLastSuccess = currentTime `diffUTCTime` peerLastValidatedAt peer
      in retryTimeout <= fromLastSuccess

    peersKnownTo :: Peer -> ServerM ([Peer], [BaseUrl])
    peersKnownTo peer = do
      knownPeers <- runExceptT $ peerKnownPeers $ peerUrl peer
      pure $ case knownPeers of
        Right peers -> ([peer], peers)
        _ -> mempty

addDefaultPeersIfNoneDiscovered :: ServerM ()
addDefaultPeersIfNoneDiscovered = do
  isNoneDiscovered <- dbQuery isNonePeersDiscovered
  when isNoneDiscovered $ do
    predefinedPeers <- descReqPredefinedPeers <$> getDiscoveryRequisites
    dbQuery $ addNewPeers $ newPeer <$> predefinedPeers

peerIntroduce :: ServerM ()
peerIntroduce = void $ runMaybeT $ do
  ownAddress <- MaybeT $ descReqOwnAddress <$> getDiscoveryRequisites
  lift $ do
    allPeers <- dbQuery $ getDiscoveredPeers
    let introduceReq = IntroducePeerReq $ showBaseUrl ownAddress
    forM_ allPeers (flip getIntroducePeerEndpoint introduceReq . peerUrl)


instance Hashable BaseUrl where
  hashWithSalt salt = hashWithSalt salt . showBaseUrl

newPeer peerUrl = NewPeer peerUrl (baseUrlScheme peerUrl)

peerInfoRequest :: BaseUrl -> ExceptT PeerValidationResult ServerM InfoResponse
peerInfoRequest baseUrl =
  ExceptT $ (const InfoEndpointError `mapLeft`) 
         <$> getInfoEndpoint baseUrl ()

peerKnownPeersRequest :: BaseUrl -> ExceptT PeerValidationResult ServerM KnownPeersResp
peerKnownPeersRequest baseUrl =
  ExceptT $ (const KnownPeersEndpointError `mapLeft`)
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

refreshKnownPeersCache :: ServerM ()
refreshKnownPeersCache = do
  stored <- dbQuery getDiscoveredPeers
  updateKnownPeers stored