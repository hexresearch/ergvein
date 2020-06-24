module Ergvein.Wallet.Worker.IndexersNetworkActualization
  (
    indexersNetworkActualizationWorker
  ) where

import Control.Monad.Reader
import Control.Monad.Zip
import Data.Bifunctor
import Data.Maybe
import Data.Time
import Reflex.ExternalRef
import Servant.Client
import System.Random.Shuffle

import Ergvein.Index.API.Types
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Types.Transaction
import Ergvein.Wallet.Client
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings

import Data.Set (Set)
import Data.Map.Strict (Map)

import qualified Data.List          as L
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import qualified Data.Text          as T

infoWorkerInterval :: NominalDiffTime
infoWorkerInterval = 60

minIndexers :: Int
minIndexers = 2

newIndexers :: (PlatformNatives, MonadIO m, HasClientManager m) => Set BaseUrl -> m (Set BaseUrl)
newIndexers knownIndexers = do
  mng <- getClientManager
  successfulResponses <- concat <$> ((`runReaderT` mng) $ mapM knownIndexersFrom $ Set.toList knownIndexers)
  let validIndexerUrls = Set.fromList $ catMaybes $ parseBaseUrl <$> successfulResponses
  pure validIndexerUrls
  where
    knownIndexersFrom url = do
      result <- getKnownPeersEndpoint url $ KnownPeersReq False
      case result of
        Right (KnownPeersResp list) -> pure list
        Left err -> do
          logWrite $ "[IndexersNetworkActualization][Getting peer list][" <> T.pack (showBaseUrl url) <> "]: " <> showt err
          pure mempty

indexersNetwork :: forall m . (PlatformNatives, MonadIO m, HasClientManager m) => Int -> [BaseUrl] -> m (Map BaseUrl IndexerInfo, Set BaseUrl)
indexersNetwork targetAmount peers =
  go peers mempty mempty
  where
    go :: [BaseUrl] -> Map BaseUrl IndexerInfo -> Set BaseUrl -> m (Map BaseUrl IndexerInfo, Set BaseUrl)
    go toExplore exploredInfoMap result
      | length result == targetAmount || null toExplore = 
        pure (exploredInfoMap, result)
      | otherwise = do
        let needed = targetAmount - length result
            available = length toExplore
            (indexers, toExplore') = splitAt (min needed available) toExplore

        newExploredInfoMap <- indexersInfo indexers

        let exploredInfoMap' = exploredInfoMap `Map.union` newExploredInfoMap
            newWorkingIndexers = Set.filter (`Map.member` exploredInfoMap') $ Set.fromList indexers
            median = medianScanInfoMap $ indInfoHeights <$> Map.elems exploredInfoMap'
            result' = Set.filter (matchMedian median . indInfoHeights . (exploredInfoMap' Map.!)) $ result `Set.union` newWorkingIndexers

        go toExplore' exploredInfoMap' result'

    matchMedian :: PeerScanInfoMap -> PeerScanInfoMap -> Bool
    matchMedian peer median = all (\currency -> predicate (peer Map.! currency) (median Map.! currency)) $ Map.keys peer
      where
        predicate (peerScannedHeight, peerActualHeight) (medianScannedHeight, medianActualHeight) =
          peerScannedHeight >= medianScannedHeight && peerActualHeight == medianActualHeight

    medianScanInfoMap :: [PeerScanInfoMap] -> PeerScanInfoMap
    medianScanInfoMap infos = let
      in bimap median' median' . munzip <$> Map.unionsWith (<>) (fmap pure <$> infos)
      where
        median' :: (Ord a) => [a] -> a
        median' v = L.sort v !! (length v `div` 2)

    indexersInfo :: [BaseUrl] -> m (Map BaseUrl IndexerInfo)
    indexersInfo urls = do
      mng <- getClientManager 
      fmap mconcat $ (`runReaderT` mng) $ mapM peerInfo urls
      where
        peerInfo url = do
          t0 <- liftIO $ getCurrentTime
          result <- getInfoEndpoint url ()
          t1 <- liftIO $ getCurrentTime
          case result of
            Right info -> do
              let pingTime = diffUTCTime t1 t0
                  scanInfo = mconcat $ mapping <$> infoScanProgress info
              pure $ Map.singleton url $ IndexerInfo scanInfo pingTime
            Left err ->  do
              logWrite $ "[IndexersNetworkActualization][Getting info][" <> T.pack (showBaseUrl url) <> "]: " <> showt err
              pure mempty
        mapping :: ScanProgressItem -> PeerScanInfoMap
        mapping (ScanProgressItem currency scanned actual) = Map.singleton currency (scanned, actual)

indexersNetworkActualizationWorker :: MonadFront t m => m ()
indexersNetworkActualizationWorker = do
  buildE            <- getPostBuild
  refreshE          <- fst  <$> getIndexerInfoEF
  te                <- void <$> tickLossyFromPostBuildTime infoWorkerInterval
  settingsRef       <- getSettingsRef
  activeUrlsRef     <- getActiveUrlsRef
  inactiveUrlsRef   <- getInactiveUrlsRef
  archivedUrlsRef   <- getArchivedUrlsRef

  let goE = leftmost [void te, refreshE, buildE]

  performFork_ $ ffor goE $ const $ do
    inactiveUrls          <- readExternalRef inactiveUrlsRef 
    archivedUrls          <- readExternalRef archivedUrlsRef
    settings              <- readExternalRef settingsRef
    currentNetworkInfoMap <- readExternalRef activeUrlsRef

    let maxIndexersToExplore = settingsActUrlNum settings
        indexersToExclude = inactiveUrls `Set.union` archivedUrls
        currentNetwork = Map.keysSet currentNetworkInfoMap

    fetchedIndexers <- newIndexers currentNetwork

    let filteredIndexers = currentNetwork `Set.union` fetchedIndexers Set.\\ indexersToExclude
    
    shuffledIndexers <- liftIO $ shuffleM $ Set.toList filteredIndexers
    (newNetworkInfoMap, newNetwork) <- indexersNetwork maxIndexersToExplore shuffledIndexers

    let resultingNetwork = if length newNetwork >= minIndexers then newNetwork else currentNetwork
        resultingNetworkInfoMap = Map.fromSet (newNetworkInfoMap Map.!?) resultingNetwork

    modifyExternalRefMaybe_ activeUrlsRef (\previous -> 
      if previous /= resultingNetworkInfoMap then
        Just resultingNetworkInfoMap 
      else Nothing)