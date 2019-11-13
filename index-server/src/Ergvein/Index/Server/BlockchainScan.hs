module Ergvein.Index.Server.BlockchainScan where

import Control.Concurrent
import Control.Immortal
import Control.Monad
import Control.Monad.IO.Unlift
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

import Ergvein.Index.Server.BlockchainCache
import Control.Concurrent.STM.TVar
import Control.Monad.STM

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

storeInfo :: DBPool -> BlockInfo -> IO ()
storeInfo dbPool blockInfo = do
  runDbQuery dbPool $ insertTxs $ block'TxInfos blockInfo
  runDbQuery dbPool $ insertTxOuts $ block'TxOutInfos blockInfo
  runDbQuery dbPool $ insertTxIns $ block'TxInInfos blockInfo
  pure ()

updateCache :: TVar BCCache -> BlockInfo -> IO () 
updateCache cache blockInfo = atomically $ modifyTVar' cache (<> B)

storeScannedHeight :: DBPool -> Currency -> BlockHeight -> IO ()
storeScannedHeight dbPool currency scannedHeight = void $ runDbQuery dbPool $ upsertScannedHeight currency scannedHeight

scannerThread :: MonadUnliftIO m => ServerEnv -> Currency -> (BlockHeight -> IO BlockInfo) -> m Thread
scannerThread env currency scanInfo = 
  create scanIteration
  where
    pool = envPool env
    blockIteration blockHeight = do
      blockInfo <- scanInfo blockHeight
      storeInfo pool blockInfo
      storeScannedHeight pool currency blockHeight
    scanIteration thread = liftIO $ do
      heights <- blockHeightsToScan env currency
      sequence_ $ blockIteration <$> heights
      threadDelay $ configBlockchainScanDelay $ envConfig env

startBlockchainScanner :: MonadUnliftIO m => ServerEnv -> m [Thread]
startBlockchainScanner env =
    sequenceA 
    [ scannerThread env BTC $ bTCBlockScanner env 
    ]