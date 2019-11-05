module Ergvein.Index.Server.BlockchainScan where

import Control.Concurrent
import Control.Immortal
import Control.Monad.IO.Unlift
import Data.Maybe
import Database.Persist.Sql

import Ergvein.Index.Server.BlockScanner.BTCBlockScanner
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

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
    [ scannerThread scanDelay (blockHeightsToScan env BTC) $ bTCBlockScanner env
    ]
    where 
      scanDelay = configBlockchainScanDelay $ envConfig env