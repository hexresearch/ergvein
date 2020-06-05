module Ergvein.Wallet.Worker.Info
  (
    infoWorker
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

infoWorker :: MonadFront t m => m ()
infoWorker = do
  buildE <- getPostBuild
  refreshE        <- fst <$> getIndexerInfoEF
  te <- void <$> tickLossyFromPostBuildTime infoWorkerInterval
  let goE = leftmost [void te, refreshE, buildE]
  activeUrlsRef   <- getActiveUrlsRef
  inactiveUrlsRef <- getInactiveUrlsRef
  archivedUrlsRef <- getArchivedUrlsRef
  
  activeUrls <- readExternalRef activeUrlsRef
  inactiveUrls <- readExternalRef inactiveUrlsRef 
  archivedUrls <- readExternalRef archivedUrlsRef
  performFork_ $ ffor goE $ const $ do
    mng <- getClientManager
    knownPeersResult <- fmap rights . (`runReaderT` mng) $ mapM (`getKnownPeersEndpoint` KnownPeersReq False) $ M.keys activeUrls
    let fetchedUrls = S.fromList $ fromJust . parseBaseUrl <$> (knownPeersList =<< knownPeersResult)
        filtered = M.keysSet activeUrls `S.union` fetchedUrls S.\\ inactiveUrls S.\\ archivedUrls
    
    shFiltered <- liftIO $ shuffleM $ S.toList filtered

    let min = 2
        max = 16
    
    (newNetwork, networkInfoMap) <- indexersNetwork max shFiltered
    
    pure ()
    {-urlChunks <- chunksOf chunkN . M.keys <$> readExternalRef indexerInfoRef
    mng <- getClientManager
    ress <- liftIO $ fmap mconcat $ (`mapConcurrently` urlChunks) $ \urls -> (`traverse` urls) $ \u -> do
      t0 <- getCurrentTime
      res <- runReaderT (getInfoEndpoint u ()) 
      t1 <- getCurrentTime
      case res of
        Left err -> do
          logWrite $ "[InfoWorker][" <> T.pack (showBaseUrl u) <> "]: " <> showt err
          pure (u, Nothing)
        Right (InfoResponse vals) -> let
          curmap = M.fromList $ (\(ScanProgressItem cur sh ah) -> (cur, (sh, ah))) <$> vals
          in pure $ (u,) $ Just $ IndexerInfo curmap $ diffUTCTime t1 t0
    writeExternalRef indexerInfoRef $ M.fromList ress_-}

indexersNetwork :: forall m . HasClientManager m => Int -> [BaseUrl] -> m (M.Map BaseUrl IndexerInfo, [BaseUrl])
indexersNetwork targetAmount peers =
  go peers mempty mempty
  where
    go :: [BaseUrl] -> M.Map BaseUrl IndexerInfo -> [BaseUrl] -> m (M.Map BaseUrl IndexerInfo, [BaseUrl])
    go toExplore exploredInfoMap result
      | length result == targetAmount || null toExplore = 
        pure (exploredInfoMap, result)
      | otherwise = do
        let needed = targetAmount - length result
            available = length toExplore
            (indexers, toExplore') = splitAt (min needed available) toExplore

        newExploredInfoMap <- indexersInfo indexers

        let exploredInfoMap' = exploredInfoMap `M.union` newExploredInfoMap
            newWorkingIndexers = filter (`M.member` exploredInfoMap') indexers
            median = medianScanInfoMap $ indInfoHeights <$> M.elems exploredInfoMap'
            result' = filter (matchMedian median . indInfoHeights . (exploredInfoMap' M.!)) $ result ++ newWorkingIndexers

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
          pure $ case result of
            Right info -> let
              pingTime = diffUTCTime t1 t0
              scanInfo = mconcat $ mapping <$> infoScanProgress info
              in M.singleton url $ IndexerInfo scanInfo pingTime
            otherwise -> mempty
        mapping :: ScanProgressItem -> PeerScanInfoMap
        mapping (ScanProgressItem currency scanned actual) = M.singleton currency (scanned, actual)