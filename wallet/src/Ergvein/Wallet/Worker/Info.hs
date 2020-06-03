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

groupMapBy :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupMapBy keySelector = M.fromListWith (++) . fmap (\v-> (keySelector v , [v]))

type PeerScanInfoMap = M.Map Currency (BlockHeight, BlockHeight) -- (scanned, actual)

extendWithNewPeers :: forall t m . MonadFront t m => Int -> (M.Map BaseUrl PeerScanInfoMap) -> [BaseUrl] -> S.Set BaseUrl -> m [BaseUrl]
extendWithNewPeers n start discovered toAvoid  = do
  go start discovered
  where
    go :: M.Map BaseUrl PeerScanInfoMap -> [BaseUrl] -> m [BaseUrl]
    go acc disc
      | length acc == n || disc == [] = 
        pure $ M.keys acc
      | otherwise = do
        let needed = n - length acc
            available = length disc
            (neededUrls, xs) = splitAt (min needed available) disc
        r <- peersInfo neededUrls
        let rupd = acc `M.union` mempty
            med = median $ M.elems rupd
            i =  all (sf med) rupd

        go rupd xs
    sf :: PeerScanInfoMap -> PeerScanInfoMap-> Bool
    sf a b = all (\x -> f (a  M.! x  ) (b M.! x)) $ M.keys a
      where
        f (sh, ah) (shm, ahm)= sh >= shm && ah == shm 
    median :: [PeerScanInfoMap] -> PeerScanInfoMap
    median arr = let
      in bimap median' median' . munzip <$> M.unionsWith (<>) (fmap pure <$> arr)
      where
        median' :: V.Vector BlockHeight -> BlockHeight
        median' a =  a V.! length a `div` 2
    mapping = M.fromList . fmap (\(ScanProgressItem cur sh ah) -> (cur, (sh, ah))). infoScanProgress

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