module Ergvein.Wallet.Worker.Info
  (
    infoWorker
  ) where

import Control.Concurrent.Async
import Control.Monad.Reader
import Data.List.Split
import Data.Time
import Data.List
import Network.HTTP.Client(Manager)
import Reflex.ExternalRef
import Servant.Client
import Data.Either
import Data.Maybe
import Control.Monad.Zip
import qualified Data.Vector as V

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
  let chunkN = 3  -- Number of concurrent request threads
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
        filtered = fetchedUrls S.\\ inactiveUrls S.\\ archivedUrls
    
    
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

type PeerScanInfoMap = M.Map Currency (BlockHeight, BlockHeight) -- (scanned, actual)

extendWithNewPeers :: forall t m . MonadFront t m => Int -> [BaseUrl] -> (M.Map BaseUrl PeerScanInfoMap) -> m [BaseUrl]
extendWithNewPeers targetAmount newPeers initial = do
  go newPeers initial
  where
    go :: [BaseUrl] -> M.Map BaseUrl PeerScanInfoMap ->  m [BaseUrl]
    go newPeersRem acc
      | length acc == targetAmount || null newPeersRem = 
        pure $ M.keys acc
      | otherwise = do
        let needed = targetAmount - length acc
            available = length newPeersRem
            (peers, newPeersRem') = splitAt (min needed available) newPeersRem
        peersInfo <- peersInfo peers
        let newAcc = acc `M.union` peersInfo
            median = medianScanInfoMap $ M.elems newAcc
            filteredToMedianNewAcc =  M.filter (matchMedian median) newAcc
        go newPeersRem' filteredToMedianNewAcc

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

    peersInfo :: MonadFront t m  => [BaseUrl] -> m (M.Map BaseUrl PeerScanInfoMap)
    peersInfo urls = do
      mng <- getClientManager 
      fmap mconcat $ (`runReaderT` mng) $ mapM peerInfo urls
      where
        peerInfo url = do
          result <- getInfoEndpoint url ()
          pure $ case result of
            Right info -> M.singleton url $ mconcat $ mapping <$> infoScanProgress info
            Left _ -> mempty
        mapping :: ScanProgressItem -> PeerScanInfoMap
        mapping (ScanProgressItem currency scanned actual) = M.singleton currency (scanned, actual)