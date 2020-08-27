module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Control.Concurrent
import Control.Immortal
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Either.Combinators
import Data.Foldable
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Time.Clock
import Servant.Client.Core

import Ergvein.Index.API.Types
import Ergvein.Index.Client.V1
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.Utils
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.TCPService.Server
import Network.Socket
import qualified Data.Map.Strict as M
import Control.Concurrent.STM


import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as HC
import qualified Data.Set as Set

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
  baseUrl <- peerBaseUrl $ peerCandidateUrl candidate
  knownPeers <- lift $ getKnownPeersList1
  knowPeersSet <- undefined
  if not $ Set.member baseUrl knowPeersSet then do
    _ <- peerKnownPeers baseUrl
    currentTime <- liftIO getCurrentTime
    let newPeer = Peer baseUrl currentTime $ baseUrlScheme baseUrl
    lift $ addKnownPeers [newPeer]
  else
    ExceptT $ pure $ Left AlreadyKnown

knownPeersSet :: [Peer1] -> ServerM (Set SockAddr)
knownPeersSet discoveredPeers = do
  ownAddress <- descReqOwnAddress <$> getDiscoveryRequisites
  pure $ Set.fromList $ (toList ownAddress) ++ (peerAddress <$> discoveredPeers)

knownPeersActualization1 :: ServerM Thread
knownPeersActualization1 = do
  create $ logOnException . scanIteration
  where
    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
     PeerDiscoveryRequisites {..} <- getDiscoveryRequisites
     currentTime <- liftIO getCurrentTime
     knownPeers <- getKnownPeersList1
     let peersToFetchFrom = isNotOutdated descReqPredefinedPeers descReqActualizationTimeout currentTime `filter` knownPeers
     setKnownPeersList1 peersToFetchFrom
     openedConnectionsRef <- asks envOpenConnections
     opened <- liftIO $ M.keysSet <$> readTVarIO openedConnectionsRef
     let toOpen = Set.toList $ opened Set.\\ (Set.fromList $ peerAddress <$> peersToFetchFrom)
     forM_ toOpen newConnection
     pure ()
    isNotOutdated :: Set SockAddr -> NominalDiffTime -> UTCTime -> Peer1 -> Bool
    isNotOutdated predefined retryTimeout currentTime peer = 
       let fromLastSuccess = currentTime `diffUTCTime` peerLastValidatedAt1 peer
       in Set.member (peerAddress peer) predefined || retryTimeout >= fromLastSuccess

knownPeersActualization :: ServerM Thread
knownPeersActualization = do
  create $ logOnException . scanIteration
  where
    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      requisites  <- getDiscoveryRequisites
      currentTime <- liftIO getCurrentTime
      knownPeers  <- getKnownPeersList1

      let peersToFetchFrom = undefined --(isNotOutdated (descReqPredefinedPeers requisites) (descReqActualizationTimeout requisites) currentTime) `filter` knownPeers
      
      knowPeersSet <- knownPeersSet peersToFetchFrom
      (failed, successful, fetched) <- mconcat <$> mapM queryKnownPeersTo peersToFetchFrom
      let uniqueFetchedUrls = undefined --(not . (`Set.member` knowPeersSet)) `filter` uniqueElements fetched
          uniqueFetched = (\x-> Peer x currentTime (baseUrlScheme x)) <$> uniqueFetchedUrls
      let refreshedSuccessful = (\x-> x {peerLastValidatedAt = currentTime}) <$> successful
      setKnownPeersList $ failed ++ refreshedSuccessful ++ uniqueFetched
      liftIO $ threadDelay $ descReqActualizationDelay requisites

    isNotOutdated :: Set BaseUrl -> NominalDiffTime -> UTCTime -> Peer -> Bool
    isNotOutdated predefined retryTimeout currentTime peer = 
      let fromLastSuccess = currentTime `diffUTCTime` peerLastValidatedAt peer
      in Set.member (peerUrl peer) predefined || retryTimeout >= fromLastSuccess

    queryKnownPeersTo :: Peer -> ServerM ([Peer], [Peer], [BaseUrl])
    queryKnownPeersTo peer = do
      knownPeers <- runExceptT $ peerKnownPeers $ peerUrl peer
      pure $ case knownPeers of
        Right peers -> ([]     , [peer] , peers)
        otherwise   -> ([peer] , []     , []   )

syncWithDefaultPeers :: ServerM ()
syncWithDefaultPeers = do
  discoveredPeers <- getKnownPeersList
  predefinedPeers <- descReqPredefinedPeers <$> getDiscoveryRequisites
  currentTime <- liftIO getCurrentTime
  let discoveredPeersSet = Set.fromList $ peerUrl <$> discoveredPeers
      toAdd = undefined --(\x -> Peer x currentTime (baseUrlScheme x)) <$> (Set.toList $ predefinedPeers Set.\\ discoveredPeersSet)
  addKnownPeers toAdd

peerIntroduce :: ServerM ()
peerIntroduce = void $ runMaybeT $ do
  ownAddress <- MaybeT $ descReqOwnAddress <$> getDiscoveryRequisites
  lift $ do
    allPeers <- getKnownPeersList
    let introduceReq = IntroducePeerReq $ showBaseUrl undefined--ownAddress
    forM_ allPeers (flip getIntroducePeerEndpoint introduceReq . peerUrl)

instance Hashable BaseUrl where
  hashWithSalt salt = hashWithSalt salt . showBaseUrl

newPeer peerUrl = NewPeer peerUrl (baseUrlScheme peerUrl)

peerBaseUrl :: String -> ExceptT PeerValidationResult ServerM BaseUrl
peerBaseUrl url = ExceptT $ pure $ maybeToRight InfoEndpointError $ parseBaseUrl url 

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
      let currencyInSync = and $ notLessThenOne localScannedHeight <$> scanProgressScannedHeight candidateNfo
      if currencyInSync then
        Right ()
      else 
        Left $ CurrencyOutOfSync $ CurrencyOutOfSyncInfo localCurrency localScannedHeight

    candidateInfoMap = Map.fromList $ (\scanInfo -> (scanProgressCurrency scanInfo, scanInfo))
                                   <$> infoScanProgress candidateInfo