module Ergvein.Index.Server.App where

import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Database.SQLite.Simple
import System.Posix.Signals

import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Environment
-- import Ergvein.Index.Server.Metrics
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Server
import Ergvein.Index.Server.Utils
import Ergvein.Index.Server.Worker.Fees
import Ergvein.Index.Server.DB.Worker
import Ergvein.Index.Server.Worker.Rates
import Ergvein.Text

import qualified Data.Text.IO as T


onStartup :: Bool -> ServerEnv -> ServerM ([Thread], [Thread])
onStartup onlyScan _ = do
  -- insertScannedBlockHeight BTC 369000
  dbWorkerThread <- dbWorker
  scanningWorkers <- blockchainScanning
  if onlyScan then pure (scanningWorkers, []) else do
    --syncWithDefaultPeers
    feeWorkers <- feesScanner
    -- kpaThread <- knownPeersActualization
    ratesThread <- ratesScanner
    tcpServerThread <- runTcpSrv
    pure $ (dbWorkerThread:scanningWorkers, tcpServerThread : ratesThread : feeWorkers)

onShutdown :: ServerEnv -> IO ()
onShutdown env = do
  T.putStrLn "Server stop signal recivied..."
  T.putStrLn "service is stopping"
  atomically $ writeTChan (envShutdownChannel env) True
  atomically $ writeTVar (envShutdownFlag env) True

finalize :: (MonadIO m, MonadLogger m) => ServerEnv -> [Thread] -> [Thread] -> m ()
finalize env scannerThreads workerTreads = do
  logInfoN "Waiting for scaner threads to close"
  liftIO $ mapM_ wait scannerThreads
  logInfoN "Waiting for other threads to close"
  liftIO $ mapM_ wait workerTreads
  logInfoN "Dumping rollback data"
  dumpRollbackDb $ envRollbackDb env
  logInfoN "Closing db connections"
  liftIO $ do
    close $ envFiltersDb env
    close $ envUtxoDb env
    close $ envRollbackDb env
  logInfoN "service is stopped"

app :: (MonadUnliftIO m, MonadLogger m) => Bool -> Config -> ServerEnv -> m ()
app onlyScan cfg env = do
  (scannerThreads, workerThreads) <- liftIO $ runServerMIO env $ onStartup onlyScan env
  -- runReaderT serveMetrics cfg
  logInfoN $ "Server started at:" <> (showt . cfgServerPort $ cfg)
  _ <- liftIO $ installHandler sigTERM (Catch $ onShutdown env) Nothing
  _ <- liftIO $ installHandler sigINT  (Catch $ onShutdown env) Nothing
  liftIO $ cancelableDelay (envShutdownFlag env) (-1)
  finalize env scannerThreads workerThreads
