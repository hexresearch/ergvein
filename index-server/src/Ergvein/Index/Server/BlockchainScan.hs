module Ergvein.Index.Server.BlockchainScan where

import Control.Concurrent
import Control.Immortal
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Database.Persist.Sql
import Network.Bitcoin.Api.Blockchain
import Network.Bitcoin.Api.Client

import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

btcNodeClient :: Config -> (Client -> IO a) -> IO a
btcNodeClient cfg = withClient 
  (configBTCNodeHost     cfg)
  (configBTCNodePort     cfg)
  (configBTCNodeUser     cfg)
  (configBTCNodePassword cfg)

scannedBlockHeight :: DBPool -> Currency -> IO (Maybe BlockHeight)
scannedBlockHeight pool currency = do
        entity <- runDbQuery pool $ getScannedHeight currency
        pure $ scannedHeightRecHeight . entityVal <$> entity

bTCBlockScanner :: ServerEnv -> BlockHeight -> IO ()
bTCBlockScanner env blockHeightToScan = do
    let cfg = envConfig env
    blockHash <- btcNodeClient cfg $ flip getBlockHash $ fromIntegral blockHeightToScan
    block <- btcNodeClient cfg $ flip getBlock blockHash
    runDbQuery (envPool env) $ updateScannedHeight BTC blockHeightToScan
    pure ()

blockHeightsToScan :: ServerEnv -> Currency -> IO [BlockHeight]
blockHeightsToScan env currency = do
    actual  <- actualHeight
    scanned <- scannedBlockHeight (envPool env) currency
    let start = fromMaybe startHeight $ succ <$> scanned
    pure [start..actual]
    where
      cfg = envConfig env
      actualHeight = fromIntegral <$> case currency of BTC  -> btcNodeClient cfg getBlockCount
                                                       ERGO -> undefined
      startHeight = case currency of BTC  -> 0
                                     ERGO -> 0

scannerThread :: MonadUnliftIO m => Int -> IO [BlockHeight] -> (BlockHeight -> IO ()) -> m Thread
scannerThread scanDelay heightsM scanner = 
    create iteration
    where
      iteration thread = liftIO $ do
        heights <- heightsM
        sequence_ $ scanner <$> heights
        threadDelay scanDelay

startBlockchainScanner :: MonadUnliftIO m => ServerEnv -> m [Thread]
startBlockchainScanner env = sequenceA 
    [
    scannerThread (configBlockchainScanDelay $ envConfig  env) (blockHeightsToScan env BTC) $ bTCBlockScanner env
    ]