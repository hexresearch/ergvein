module Ergvein.Wallet.Worker.IndexersNetworkActualization
  (
    indexersNetworkActualizationWorker
  ) where

import Control.Monad.Reader
import Control.Monad.Zip
import Data.Bifunctor
import Data.List.Split
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

import qualified Data.List   as L
import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.Text   as T
import qualified Data.Vector as V

infoWorkerInterval :: NominalDiffTime
infoWorkerInterval = 60

minIndexers :: Int
minIndexers = 2

maxIndexers :: Int
maxIndexers = 16

indexersToExclude :: MonadFront t m => m (S.Set BaseUrl)
indexersToExclude = do
  inactiveUrlsRef <- getInactiveUrlsRef
  archivedUrlsRef <- getArchivedUrlsRef
  inactiveUrls <- readExternalRef inactiveUrlsRef 
  archivedUrls <- readExternalRef archivedUrlsRef
  pure $ inactiveUrls `S.union` archivedUrls

newIndexers :: (PlatformNatives, MonadIO m, HasClientManager m) => S.Set BaseUrl -> m (S.Set BaseUrl)
newIndexers knownIndexers = do
  mng <- getClientManager
  successfulResponses <- concat <$> ((`runReaderT` mng) $ mapM knownIndexersFrom $ S.toList knownIndexers)
  let validIndexerUrls = S.fromList $ catMaybes $ parseBaseUrl <$> successfulResponses
  pure validIndexerUrls
  where
    knownIndexersFrom url = do
      result <- getKnownPeersEndpoint url $ KnownPeersReq False
      case result of
        Right (KnownPeersResp list) -> pure list
        Left err -> do
          logWrite $ "[IndexersNetworkActualization][Getting peer list][" <> T.pack (showBaseUrl url) <> "]: " <> showt err
          pure mempty

indexersNetwork :: forall m . (PlatformNatives, MonadIO m, HasClientManager m) => Int -> [BaseUrl] -> m (M.Map BaseUrl IndexerInfo, S.Set BaseUrl)
indexersNetwork targetAmount peers =
  go peers mempty mempty
  where
    go :: [BaseUrl] -> M.Map BaseUrl IndexerInfo -> S.Set BaseUrl -> m (M.Map BaseUrl IndexerInfo, S.Set BaseUrl)
    go toExplore exploredInfoMap result
      | length result == targetAmount || null toExplore = 
        pure (exploredInfoMap, result)
      | otherwise = do
        let needed = targetAmount - length result
            available = length toExplore
            (indexers, toExplore') = splitAt (min needed available) toExplore

        newExploredInfoMap <- indexersInfo indexers

        let exploredInfoMap' = exploredInfoMap `M.union` newExploredInfoMap
            newWorkingIndexers = S.filter (`M.member` exploredInfoMap') $ S.fromList indexers
            median = medianScanInfoMap $ indInfoHeights <$> M.elems exploredInfoMap'
            result' = S.filter (matchMedian median . indInfoHeights . (exploredInfoMap' M.!)) $ result `S.union` newWorkingIndexers

        go toExplore' exploredInfoMap' result'

    matchMedian :: PeerScanInfoMap -> PeerScanInfoMap -> Bool
    matchMedian peer median = all (\currency -> predicate (peer M.! currency) (median M.! currency)) $ M.keys peer
      where
        predicate (peerScannedHeight, peerActualHeight) (medianScannedHeight, medianActualHeight) =
          peerScannedHeight >= medianScannedHeight && peerActualHeight == medianActualHeight

    medianScanInfoMap :: [PeerScanInfoMap] -> PeerScanInfoMap
    medianScanInfoMap infos = let
      in bimap median' median' . munzip <$> M.unionsWith (<>) (fmap pure <$> infos)
      where
        median' :: V.Vector BlockHeight -> BlockHeight
        median' v =  v V.! (length v `div` 2)

    indexersInfo :: [BaseUrl] -> m (M.Map BaseUrl IndexerInfo)
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
              pure $  M.singleton url $ IndexerInfo scanInfo pingTime
            Left err ->  do
              logWrite $ "[IndexersNetworkActualization][Getting info][" <> T.pack (showBaseUrl url) <> "]: " <> showt err
              pure mempty
        mapping :: ScanProgressItem -> PeerScanInfoMap
        mapping (ScanProgressItem currency scanned actual) = M.singleton currency (scanned, actual)

indexersNetworkActualizationWorker :: MonadFront t m => m ()
indexersNetworkActualizationWorker = do
  buildE   <- getPostBuild
  refreshE <- fst  <$> getIndexerInfoEF
  te       <- void <$> tickLossyFromPostBuildTime infoWorkerInterval

  let goE = leftmost [void te, refreshE, buildE]

  activeUrlsRef          <- getActiveUrlsRef
  indexersToExclude      <- indexersToExclude

  performFork_ $ ffor goE $ const $ do
    currentNetworkInfoMap  <- readExternalRef activeUrlsRef

    let currentNetwork = M.keysSet currentNetworkInfoMap

    fetchedIndexers <- newIndexers currentNetwork

    let filteredIndexers = currentNetwork `S.union` fetchedIndexers S.\\ indexersToExclude
    
    shuffledIndexers <- liftIO $ shuffleM $ S.toList filteredIndexers
    (newNetworkInfoMap, newNetwork) <- indexersNetwork maxIndexers shuffledIndexers

    let resultingNetwork = if length newNetwork >= minIndexers then newNetwork else currentNetwork
        resultingNetworkInfoMap = M.fromSet (newNetworkInfoMap M.!?) resultingNetwork

    writeExternalRef activeUrlsRef resultingNetworkInfoMap