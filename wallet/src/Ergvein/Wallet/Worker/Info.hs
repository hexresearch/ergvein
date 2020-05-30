module Ergvein.Wallet.Worker.Info
  (
    infoWorker
  ) where

import Control.Concurrent.Async
import Control.Monad.Reader
import Data.List.Split
import Data.Time
import Network.HTTP.Client(Manager)
import Reflex.ExternalRef
import Servant.Client
import Data.Either
import Data.Maybe

import Ergvein.Index.API.Types
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Wallet.Client
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native

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
