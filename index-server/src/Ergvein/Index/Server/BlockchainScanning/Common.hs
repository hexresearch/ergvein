module Ergvein.Index.Server.BlockchainScanning.Common where

import Control.Concurrent
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Foldable (traverse_)
import Conversion
import Data.Maybe
import Database.Persist.Sql

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Cache
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Text

import qualified Ergvein.Index.Server.BlockchainScanning.Bitcoin as BTCScanning
import qualified Ergvein.Index.Server.BlockchainScanning.Ergo as ERGOScanning

scannedBlockHeight :: (MonadIO m) => Currency -> QueryT m (Maybe BlockHeight)
scannedBlockHeight currency = do
  entity <- getScannedHeight currency
  pure $ scannedHeightRecHeight . entityVal <$> entity

blockHeightsToScan :: ServerEnv -> Currency -> IO [BlockHeight]
blockHeightsToScan env currency = do
  actual  <- blockTotalHeight env currency
  scanned <- runDbQuery (envPersistencePool env) $ scannedBlockHeight currency
  let start = maybe startHeight succ scanned
  pure [start..actual]
  where
    cfg = envServerConfig env
    startHeight =  case currency of BTC  -> 0
                                    ERGO -> 1

blockTotalHeight :: MonadIO m => ServerEnv -> Currency -> m BlockHeight
blockTotalHeight env currency = liftIO $ case currency of
    BTC  -> BTCScanning.actualHeight $ envServerConfig env
    ERGO -> ERGOScanning.actualHeight env


storeInfo :: (MonadIO m) => BlockInfo -> QueryT m ()
storeInfo blockInfo = do
  insertTxs $ blockContentTxInfos $ blockInfoContent blockInfo
  insertTxOuts $ blockContentTxOutInfos $ blockInfoContent blockInfo
  insertTxIns $ blockContentTxInInfos $ blockInfoContent blockInfo
  insertBlock $ blockInfoMeta blockInfo
  pure ()

storeScannedHeight :: (MonadIO m) => Currency -> BlockHeight -> QueryT m ()
storeScannedHeight currency scannedHeight = void $ upsertScannedHeight currency scannedHeight

scannerThread :: forall m . (MonadUnliftIO m, MonadCatch m, MonadLogger m) => ServerEnv -> Currency -> (BlockHeight -> IO BlockInfo) -> m Thread
scannerThread env currency scanInfo =
  create $ logOnException . scanIteration
  where
    pool = envPersistencePool env

    blockIteration :: BlockHeight -> BlockHeight -> m ()
    blockIteration totalh blockHeight = do
      let percent = fromIntegral blockHeight / fromIntegral totalh :: Double
      logInfoN $ "Scanning height for " <> showt currency <> " " <> showt blockHeight <> " (" <> showf 2 (100*percent) <> "%)"
      liftIO $ do
        blockInfo <- scanInfo blockHeight
        let blockInfoToStore = selectedInfoToStore blockInfo
        runDbQuery pool $ do
          storeInfo blockInfoToStore
          storeScannedHeight currency blockHeight
        addToCache (envLevelDBContext env) blockInfoToStore

    scanIteration :: Thread -> m ()
    scanIteration thread = do
      totalh <- blockTotalHeight env currency
      heights <- liftIO $ blockHeightsToScan env currency
      traverse_ (blockIteration totalh) heights
      liftIO $ threadDelay $ configBlockchainScanDelay $ envServerConfig env
    
    selectedInfoToStore info = if configPubScriptHistoryScan $ envServerConfig env then info else 
      let blockContent = BlockContentInfo (blockContentTxInfos $ blockInfoContent info) [] []
      in info { blockInfoContent = blockContent }

startBlockchainScanner :: (MonadUnliftIO m, MonadCatch m, MonadLogger m) => ServerEnv -> m [Thread]
startBlockchainScanner env =
    sequenceA
    [ scannerThread env BTC  $ BTCScanning.blockInfo  env
    , scannerThread env ERGO $ ERGOScanning.blockInfo env 
    ]