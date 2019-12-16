module Ergvein.Index.Server.BlockchainScan where

import Control.Concurrent
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Maybe
import Database.Persist.Sql

import Ergvein.Index.Server.BlockScanner.BTCBlockScanner
import Ergvein.Index.Server.BlockScanner.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import Ergvein.Index.Server.Cache
import Conversion


import qualified Data.Text.IO as T
import Data.Text (Text, pack)

scannedBlockHeight :: DBPool -> Currency -> IO (Maybe BlockHeight)
scannedBlockHeight pool currency = do
  entity <- runDbQuery pool $ getScannedHeight currency
  pure $ scannedHeightRecHeight . entityVal <$> entity

blockHeightsToScan :: ServerEnv -> Currency -> IO [BlockHeight]
blockHeightsToScan env currency = do
  actual  <- actualHeight
  scanned <- scannedBlockHeight (envPool env) currency
  let start = fromMaybe startHeight $ succ <$> scanned
  pure [start..actual]
  where
    cfg = envConfig env
    actualHeight = case currency of BTC  -> actualBTCHeight cfg
                                    ERGO -> undefined
    startHeight =  case currency of BTC  -> 0
                                    ERGO -> 0

storeInfo :: (MonadIO m) => BlockInfo -> QueryT m ()
storeInfo blockInfo = do
  insertTxs $ blockContent'TxInfos $ blockInfo'content blockInfo
  insertTxOuts $ blockContent'TxOutInfos $ blockInfo'content blockInfo
  insertTxIns $ blockContent'TxInInfos $ blockInfo'content blockInfo
  insertBlock $ blockInfo'meta blockInfo
  pure ()

storeScannedHeight :: (MonadIO m) => Currency -> BlockHeight -> QueryT m ()
storeScannedHeight currency scannedHeight = void $ upsertScannedHeight currency scannedHeight

scannerThread :: (MonadUnliftIO m, MonadCatch m, MonadLogger m) => ServerEnv -> Currency -> (BlockHeight -> IO BlockInfo) -> m Thread
scannerThread env currency scanInfo =
  create $ logOnException . scanIteration
  where
    pool = envPool env
    blockIteration blockHeight = do
      blockInfo <- scanInfo blockHeight
      runDbQuery pool $ do
        storeInfo blockInfo
        storeScannedHeight currency blockHeight
      dir <- levelDbDir
      addToCache (ldb env) blockInfo
    scanIteration thread = liftIO $ do
      heights <- blockHeightsToScan env currency
      forM_ heights blockIteration
      threadDelay $ configBlockchainScanDelay $ envConfig env

startBlockchainScanner :: (MonadUnliftIO m, MonadCatch m, MonadLogger m) => ServerEnv -> m [Thread]
startBlockchainScanner env =
    sequenceA
    [ scannerThread env BTC $ bTCBlockScanner env
    ]
