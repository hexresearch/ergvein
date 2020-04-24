module Ergvein.Wallet.Worker.Info
  (
    infoWorker
  ) where

import Control.Concurrent.Async
import Control.Monad.Reader
import Data.Time
import Network.HTTP.Client(Manager)
import Reflex.ExternalRef
import Servant.Client

import Ergvein.Index.API.Types
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Wallet.Client
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

infoWorkerInterval :: NominalDiffTime
infoWorkerInterval = 60

infoWorker :: MonadFront t m => m ()
infoWorker = do
  indexerInfoRef  <- getActiveUrlsRef
  refreshE        <- fmap fst $ getIndexerInfoEF
  te <- fmap void $ tickLossyFromPostBuildTime infoWorkerInterval
  let goE = leftmost [void te, refreshE]
  let chunkN = 3  -- Number of concurrent request threads
  performFork_ $ ffor goE $ const $ do
    urlChunks <- fmap (mkChunks chunkN . M.keys) $ readExternalRef indexerInfoRef
    mng <- getClientManager
    ress <- liftIO $ fmap mconcat $ flip mapConcurrently urlChunks $ \urls -> flip traverse urls $ \u -> do
      t0 <- getCurrentTime
      res <- runReaderT (getInfoEndpoint u ()) mng
      t1 <- getCurrentTime
      case res of
        Left err -> do
          logWrite $ "[InfoWorker][" <> T.pack (showBaseUrl u) <> "]: " <> showt err
          pure (u, Nothing)
        Right (InfoResponse vals) -> let
          curmap = M.fromList $ fmap (\(ScanProgressItem cur sh ah) -> (cur, (sh, ah))) vals
          in pure $ (u,) $ Just $ IndexerInfo curmap $ diffUTCTime t1 t0
    writeExternalRef indexerInfoRef $ M.fromList ress

mkChunks :: Int -> [a] -> [[a]]
mkChunks n vals = splitter [fstChunk] rest
  where
    l = length vals
    cl = l `div` n
    (fstChunk, rest) = splitAt (cl + l `mod` n) vals
    splitter acc xs = case xs of
      [] -> acc
      _  -> let (x,y) = splitAt cl xs in splitter (acc ++ [x]) y
