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
import  Control.Monad.Reader
import Ergvein.Index.Server.Monad
import qualified Network.Bitcoin.Api.Client  as BitcoinApi

import qualified Ergvein.Index.Server.BlockchainScanning.Bitcoin as BTCScanning
import qualified Ergvein.Index.Server.BlockchainScanning.Ergo as ERGOScanning

scannedBlockHeight :: (MonadIO m) => Currency -> QueryT m (Maybe BlockHeight)
scannedBlockHeight currency = do
  entity <- getScannedHeight currency
  pure $ scannedHeightRecHeight . entityVal <$> entity

blockHeightsToScan :: Currency -> ServerM [BlockHeight]
blockHeightsToScan currency = do
  actual  <- actualHeight currency
  scanned <- runDb $ scannedBlockHeight currency
  let start = maybe startHeight succ scanned
  pure [start..actual]
  where
    startHeight =  case currency of BTC  -> 0
                                    ERGO -> 1

actualHeight :: Currency -> ServerM BlockHeight
actualHeight currency = case currency of
    BTC  -> BTCScanning.actualHeight
    ERGO -> ERGOScanning.actualHeight 

storeInfo :: (MonadIO m) => BlockInfo -> QueryT m ()
storeInfo blockInfo = do
  insertTxs $ blockContentTxInfos $ blockInfoContent blockInfo
  insertTxOuts $ blockContentTxOutInfos $ blockInfoContent blockInfo
  insertTxIns $ blockContentTxInInfos $ blockInfoContent blockInfo
  insertBlock $ blockInfoMeta blockInfo
  pure ()

storeScannedHeight :: (MonadIO m) => Currency -> BlockHeight -> QueryT m ()
storeScannedHeight currency scannedHeight = void $ upsertScannedHeight currency scannedHeight

scannerThread :: ServerEnv -> Currency -> (BlockHeight -> IO BlockInfo) -> ServerM Thread
scannerThread env currency scanInfo =
  create $ logOnException . scanIteration
  where
    pool = envPersistencePool env

    blockIteration :: BlockHeight -> BlockHeight -> ServerM ()
    blockIteration totalh blockHeight = do
      let percent = fromIntegral blockHeight / fromIntegral totalh :: Double
      logInfoN $ "Scanning height for " <> showt currency <> " " <> showt blockHeight <> " (" <> showf 2 (100*percent) <> "%)"
      do
        blockInfo <- liftIO $ scanInfo blockHeight
        let blockInfoToStore = selectedInfoToStore blockInfo
        runDb $ do
          storeInfo blockInfoToStore
          storeScannedHeight currency blockHeight
        addToCache (envLevelDBContext env) blockInfoToStore

    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      totalh <- actualHeight currency
      heights <- blockHeightsToScan currency
      traverse_ (blockIteration totalh) heights
      liftIO $ threadDelay $ configBlockchainScanDelay $ envServerConfig env
    
    selectedInfoToStore info = if configPubScriptHistoryScan $ envServerConfig env then info else 
      let blockContent = BlockContentInfo (blockContentTxInfos $ blockInfoContent info) [] []
      in info { blockInfoContent = blockContent }

f :: (MonadIO m) => ServerEnv -> BlockHeight -> m BlockInfo
f env = (\z -> do
  x <- liftIO $ runServerMIO env $ BTCScanning.blockInfo z
  pure x)



startBlockchainScanner ::  ServerEnv -> ServerM [Thread]
startBlockchainScanner env =
    sequenceA
    [ scannerThread env BTC $ f env
    --, scannerThread env ERGO $ ERGOScanning.blockInfo env 
    ]