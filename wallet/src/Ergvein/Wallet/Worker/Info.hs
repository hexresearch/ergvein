module Ergvein.Wallet.Worker.Info
  (
    infoWorker
  ) where

import Control.Monad.Reader
import Data.Time
import Ergvein.Text
import Ergvein.Wallet.Native
import Ergvein.Index.API.Types
import Ergvein.Index.Client
import Ergvein.Wallet.Monad
import Reflex.ExternalRef

import qualified Data.Map as M

infoWorkerInterval :: NominalDiffTime
infoWorkerInterval = 60

infoWorker :: MonadFrontBase t m => m ()
infoWorker = do
  indexerInfoRef  <- getActiveUrlsRef
  mng             <- getClientMaganer
  refreshE        <- fmap fst $ getIndexerInfoEF
  te <- fmap void $ tickLossyFromPostBuildTime infoWorkerInterval
  let goE = leftmost [void te, refreshE]
  performEvent_ $ ffor goE $ const $ liftIO $ do
    urls <- fmap M.keys $ readExternalRef indexerInfoRef
    ress <- flip traverse urls $ \u -> do
      t0 <- getCurrentTime
      res <- runReaderT (getInfoEndpoint u ()) mng
      t1 <- getCurrentTime
      pure $ (u,) $ case res of
        Left _ -> Nothing
        Right (InfoResponse vals) -> let
          curmap = M.fromList $ fmap (\(ScanProgressItem cur sh ah) -> (cur, (sh, ah))) vals
          in Just $ IndexerInfo curmap $ diffUTCTime t1 t0
    writeExternalRef indexerInfoRef $ M.fromList ress
    -- logWrite $ showt ress
