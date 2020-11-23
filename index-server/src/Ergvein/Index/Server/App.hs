module Ergvein.Index.Server.App where

import Control.Concurrent.STM.TVar
import Control.Immortal
import Control.Monad.STM
import System.Posix.Signals
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader

import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Queries (storeRollbackSequence)
import Ergvein.Index.Server.DB.Schema.Indexer (RollbackSequence(..))
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Metrics
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Server
import Ergvein.Index.Server.Utils
import Ergvein.Text
import Ergvein.Types.Currency

import qualified Data.Text.IO as T

onStartup :: Bool -> ServerEnv -> ServerM ([Thread], [Thread])
onStartup onlyScan _ = do
  scanningWorkers <- blockchainScanning
  if onlyScan then pure (scanningWorkers, []) else do
    --syncWithDefaultPeers
    feeWorkers <- feesScanning
    -- kpaThread <- knownPeersActualization
    tcpServerThread <- runTcpSrv
    pure $ (scanningWorkers, tcpServerThread : feeWorkers)

onShutdown :: ServerEnv -> IO ()
onShutdown env = do
  T.putStrLn "Server stop signal recivied..."
  T.putStrLn "service is stopping"
  atomically $ writeTVar (envShutdownFlag env) True

finalize :: (MonadIO m, MonadLogger m) => ServerEnv -> [Thread] -> [Thread] -> m ()
finalize env scannerThreads workerTreads = do
  logInfoN "Waiting for scaner threads to close"
  liftIO $ sequence_ $ wait <$> scannerThreads
  logInfoN "Dumping rollback data"
  rse <- fmap RollbackSequence $ liftIO $ readTVarIO (envBtcRollback env)
  runReaderT (storeRollbackSequence BTC rse) (envIndexerDBContext env)
  logInfoN "Waiting for other threads to close"
  liftIO $ sequence_ $ wait <$> workerTreads
  logInfoN "service is stopped"

app :: (MonadUnliftIO m, MonadLogger m) => Bool -> Config -> ServerEnv -> m ()
app onlyScan cfg env = do
  (scannerThreads, workerThreads) <- liftIO $ runServerMIO env $ onStartup onlyScan env
  runReaderT serveMetrics cfg
  logInfoN $ "Server started at:" <> (showt . cfgServerPort $ cfg)
  liftIO $ installHandler sigTERM (Catch $ onShutdown env) Nothing
  liftIO $ cancelableDelay (envShutdownFlag env) (-1)
  finalize env scannerThreads workerThreads
