module Ergvein.Wallet.Worker.IndexersNetworkActualization
  (
    indexersNetworkActualizationWorker
  ) where

import Control.Concurrent.Async
import Control.Monad.Reader
import Data.List.Split
import Data.Time
import Data.List
import Data.Maybe
import Network.HTTP.Client(Manager)
import Reflex.ExternalRef
import Servant.Client
import Data.Either
import Data.Maybe
import Control.Monad.Zip
import qualified Data.Vector as V
import Ergvein.Wallet.Monad.Base

import Ergvein.Index.API.Types
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Wallet.Client
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Data.Bifunctor
import System.Random.Shuffle

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Text as T

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

newIndexers :: HasClientManager m => S.Set BaseUrl -> m (S.Set BaseUrl)
newIndexers knownIndexers = do
  mng <- getClientManager
  successfulResponses <- (`runReaderT` mng) $ fmap rights <$> mapM (`getKnownPeersEndpoint` KnownPeersReq False) $ S.toList knownIndexers
  let validIndexerUrls = S.fromList $ catMaybes $ parseBaseUrl <$> (knownPeersList =<< successfulResponses)
  pure validIndexerUrls

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
        median' v =  v V.! length v `div` 2

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
              logWrite $ "[InfoWorker][" <> T.pack (showBaseUrl url) <> "]: " <> showt err
              pure mempty
        mapping :: ScanProgressItem -> PeerScanInfoMap
        mapping (ScanProgressItem currency scanned actual) = M.singleton currency (scanned, actual)

indexersNetworkActualizationWorker :: MonadFront t m => m ()
indexersNetworkActualizationWorker = do
  buildE <- getPostBuild
  refreshE <- fst  <$> getIndexerInfoEF
  te       <- void <$> tickLossyFromPostBuildTime infoWorkerInterval

  let goE = leftmost [void te, refreshE, buildE]

  activeUrlsRef          <- getActiveUrlsRef
  currentNetworkInfoMap  <- readExternalRef activeUrlsRef
  indexersToExclude      <- indexersToExclude

  performFork_ $ ffor goE $ const $ do
    let currentNetwork = M.keysSet currentNetworkInfoMap

    fetchedIndexers <- newIndexers currentNetwork

    let filteredIndexers = currentNetwork `S.union` fetchedIndexers S.\\ indexersToExclude
    
    shuffledIndexers <- liftIO $ shuffleM $ S.toList filteredIndexers
    (newNetworkInfoMap, newNetwork) <- indexersNetwork maxIndexers shuffledIndexers

    let resultingNetwork = if length newNetwork >= minIndexers then newNetwork else currentNetwork
        resultingNetworkInfoMap = M.fromSet (newNetworkInfoMap M.!?) resultingNetwork

    writeExternalRef activeUrlsRef resultingNetworkInfoMap