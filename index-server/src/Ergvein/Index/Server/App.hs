module Ergvein.Index.Server.App where

import Control.Concurrent.STM.TVar
import Control.Immortal
import Control.Monad
import Control.Monad.STM
import Data.Text (Text, pack)
import System.Posix.Signals
import Control.Monad.IO.Unlift
import Control.Monad.Logger

import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Discovery
import Ergvein.Index.Server.TCPService.Server
import Ergvein.Index.Server.Utils
import Ergvein.Text

import qualified Data.Text.IO as T

onStartup' :: ServerEnv -> ServerM [Thread]
onStartup' env = fmap pure runTcpSrv

onStartup :: ServerEnv -> ServerM [Thread]
onStartup env = do
  scanningWorkers <- blockchainScanning
  syncWithDefaultPeers
  feeWorkers <- feesScanning
  peerIntroduce
  knownPeersActualization
  tcpServerThread <- runTcpSrv
  pure $ tcpServerThread : scanningWorkers ++ feeWorkers

onShutdown :: ServerEnv -> [Thread] -> IO ()
onShutdown env workerTreads = do
  T.putStrLn $ showt "Server stop signal recivied..."
  T.putStrLn $ showt "service is stopping"
  atomically $ writeTVar (envShutdownFlag env) True

finalize :: (MonadIO m, MonadLogger m) => [Thread] -> m ()
finalize workerTreads = do
  liftIO $ sequence_ $ wait <$> workerTreads
  logInfoN "service is stopped"

app :: (MonadIO m, MonadLogger m) => Config -> ServerEnv -> m ()
app cfg env = do
  workerThreads <- liftIO $ runServerMIO env $ onStartup env
  logInfoN $ "Server started at:" <> (showt . cfgServerPort $ cfg)
  liftIO $ installHandler sigTERM (Catch $ onShutdown env workerThreads) Nothing
  liftIO $ cancelableDelay (envShutdownFlag env) (-1)
  finalize workerThreads
