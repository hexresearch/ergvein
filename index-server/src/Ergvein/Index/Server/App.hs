module Ergvein.Index.Server.App where

import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Sequence (Seq(..))
import System.Posix.Signals

import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema.Indexer (RollbackSequence(..), RollbackRecItem(..))
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Metrics
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Server
import Ergvein.Index.Server.Utils
import Ergvein.Index.Server.Worker.Fees
import Ergvein.Index.Server.Worker.Rates
import Ergvein.Text
import Ergvein.Types.Currency

import qualified Data.Text.IO as T


-- If something bad happens, like OOM, or sigKILL and the write is interrupted
-- we might have an inconsistent database
-- The bad thing affects only the last scanned block:
-- either it's height was written but not the Txs
-- or its hash was written but not its height
-- So we always start from the block before the last one
--
-- If we have no rollback info -- just decrease the height by 1
-- If we have rollback (meaning the server was shut down properly at some point)
-- then check if it's consistent with the chain so far and then use it to keep track of the rollbacks
-- if it's not consistent -- delete the sequence and just decrease the height
--
-- The worst case scenario: indexer double scans some blocks which is not an issue
-- or it misses a fork (a rare event itself) in a very specific conditions:
-- a) a fork happend
-- b) indexer ended up on a soon to be deprecated branch
-- c) while on the branch, indexer went offline
-- b) indexer came back online only after the fork collapsed and consensus was reached
-- If all of the above happen, you will have to resync the indexer
-- It's better to resync the indexer anytime a fork happens (once on several years) anyway
dirtyBtcHack :: ServerM ()
dirtyBtcHack = do
  h <- fmap (fromMaybe 0) $ getScannedHeight BTC
  let noRoll = do
        setScannedHeight BTC (h - 1)
        deleteLastScannedBlock BTC
        storeRollbackSequence BTC $ RollbackSequence mempty

  when (h > 0) $ void $ do
    rse <- liftIO . readTVarIO =<< asks envBtcRollback
    case rse of
      Empty -> noRoll
      tip :<| rest -> if rollbackPrevHeight tip /= h - 1
        then noRoll else do
          setLastScannedBlock BTC $ rollbackPrevBlockHash tip
          setScannedHeight BTC $ rollbackPrevHeight tip
          storeRollbackSequence BTC $ RollbackSequence rest

onStartup :: Bool -> Bool -> ServerEnv -> ServerM ([Thread], [Thread])
onStartup onlyScan skipBtcHack _ = do
  unless skipBtcHack dirtyBtcHack
  scanningWorkers <- blockchainScanning
  if onlyScan then pure (scanningWorkers, []) else do
    --syncWithDefaultPeers
    feeWorkers <- feesScanner
    -- kpaThread <- knownPeersActualization
    ratesThread <- ratesScanner
    tcpServerThread <- runTcpSrv
    pure $ (scanningWorkers, tcpServerThread : ratesThread : feeWorkers)

onShutdown :: ServerEnv -> IO ()
onShutdown env = do
  T.putStrLn "Server stop signal recivied..."
  T.putStrLn "service is stopping"
  atomically $ writeTChan (envShutdownChannel env) True
  atomically $ writeTVar (envShutdownFlag env) True

finalize :: (MonadIO m, MonadLogger m) => ServerEnv -> [Thread] -> [Thread] -> m ()
finalize env scannerThreads workerTreads = do
  logInfoN "Dumping rollback data"
  rse <- fmap RollbackSequence $ liftIO $ readTVarIO (envBtcRollback env)
  runReaderT (storeRollbackSequence BTC rse) (envIndexerDBContext env)
  logInfoN "Waiting for scaner threads to close"
  liftIO $ mapM_ wait scannerThreads
  logInfoN "Waiting for other threads to close"
  liftIO $ mapM_ wait workerTreads
  logInfoN "service is stopped"

app :: (MonadUnliftIO m, MonadLogger m) => Bool -> Bool -> Config -> ServerEnv -> m ()
app onlyScan skipBtcHack cfg env = do
  (scannerThreads, workerThreads) <- liftIO $ runServerMIO env $ onStartup onlyScan skipBtcHack env
  runReaderT serveMetrics cfg
  logInfoN $ "Server started at:" <> (showt . cfgServerPort $ cfg)
  _ <- liftIO $ installHandler sigTERM (Catch $ onShutdown env) Nothing
  _ <- liftIO $ installHandler sigINT  (Catch $ onShutdown env) Nothing
  liftIO $ cancelableDelay (envShutdownFlag env) (-1)
  finalize env scannerThreads workerThreads
