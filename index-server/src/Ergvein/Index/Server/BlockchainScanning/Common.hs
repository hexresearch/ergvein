module Ergvein.Index.Server.BlockchainScanning.Common where

import Control.Concurrent
import Control.Immortal
import Control.Monad
import Control.Monad.IO.Unlift
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

import qualified Ergvein.Index.Server.BlockchainScanning.Bitcoin as BTCScanning
import qualified Ergvein.Index.Server.BlockchainScanning.Ergo as ERGOScanning

scannedBlockHeight :: (MonadIO m) => Currency -> QueryT m (Maybe BlockHeight)
scannedBlockHeight currency = do
  entity <- getScannedHeight currency
  pure $ scannedHeightRecHeight . entityVal <$> entity

blockHeightsToScan :: ServerEnv -> Currency -> IO [BlockHeight]
blockHeightsToScan env currency = do
  actual  <- actualHeight
  scanned <- runDbQuery (env'persistencePool env) $ scannedBlockHeight currency
  let start = fromMaybe startHeight $ succ <$> scanned
  pure [start..actual]
  where
    cfg = env'config env
    actualHeight = case currency of BTC  -> BTCScanning.actualHeight cfg
                                    ERGO -> ERGOScanning.actualHeight cfg 
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

scannerThread :: MonadUnliftIO m => ServerEnv -> Currency -> (BlockHeight -> IO BlockInfo) -> m Thread
scannerThread env currency scanInfo = 
  create scanIteration
  where
    pool = env'persistencePool env
    blockIteration blockHeight = do
      blockInfo <- scanInfo blockHeight
      runDbQuery pool $ do
        storeInfo blockInfo
        storeScannedHeight currency blockHeight
      pure ()
      addToCache (env'levelDBContext env) blockInfo
    scanIteration thread = liftIO $ do
      heights <- blockHeightsToScan env currency
      forM_ heights blockIteration
      threadDelay $ configBlockchainScanDelay $ env'config env

startBlockchainScanning :: MonadUnliftIO m => ServerEnv -> m [Thread]
startBlockchainScanning env =
    sequenceA 
    [ scannerThread env BTC $ BTCScanning.blockInfo env
    , scannerThread env ERGO $ ERGOScanning.blockInfo env 
    ]