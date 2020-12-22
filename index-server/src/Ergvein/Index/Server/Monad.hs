module Ergvein.Index.Server.Monad where

import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Prometheus (MonadMonitor(..))

import Ergvein.Index.Client
import Ergvein.Index.Protocol.Types (Message)
import Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment

import qualified Data.Map.Strict as M
import qualified Network.Ergo.Api.Client     as ErgoApi

newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader ServerEnv, MonadThrow, MonadCatch, MonadMask, MonadBase IO, MonadMonitor)

instance MonadMonitor m => MonadMonitor (LoggingT m) where
  doIO = lift . doIO
  {-# INLINE doIO #-}

newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT ServerEnv (LoggingT IO)) a }

instance MonadBaseControl IO ServerM where
  type StM ServerM a = StMServerM a
  liftBaseWith f = ServerM $ liftBaseWith $ \q -> f (fmap StMServerM . q . unServerM)
  restoreM = ServerM . restoreM . unStMServerM

runServerMIO :: ServerEnv -> ServerM a -> IO a
runServerMIO e = runChanLoggingT (envLogger e) . flip runReaderT e . unServerM

instance HasFiltersDB ServerM where
  getFiltersDb = asks envFiltersDBContext
  {-# INLINE getFiltersDb #-}

instance HasIndexerDB ServerM where
  getIndexerDb = asks envIndexerDBContext
  {-# INLINE getIndexerDb #-}

instance HasBtcRollback ServerM where
  getBtcRollbackVar = asks envBtcRollback
  {-# INLINE getBtcRollbackVar #-}

instance ErgoApi.ApiMonad ServerM where
  getClient = asks envErgoNodeClient
  {-# INLINE getClient #-}

instance HasBitcoinNodeNetwork ServerM where
  currentBitcoinNetwork = asks envBitcoinNodeNetwork
  {-# INLINE currentBitcoinNetwork #-}

instance HasServerConfig ServerM where
  serverConfig = asks envServerConfig
  {-# INLINE serverConfig #-}

instance BitcoinApiMonad ServerM where
  nodeRpcCall f = liftIO . f =<< asks envBitcoinClient
  {-# INLINE nodeRpcCall #-}
  getSocketConn = asks envBitcoinSocket
  {-# INLINE getSocketConn #-}
  getBtcConnectionScheme = liftIO . readTVarIO =<< asks envBtcConScheme
  {-# INLINE getBtcConnectionScheme #-}
  restartSocketConn = do
    f <- asks envBitcoinSocketReconnect
    liftIO $ f

instance HasClientManager ServerM where
  getClientManager = asks envClientManager
  {-# INLINE getClientManager #-}

instance HasDiscoveryRequisites ServerM where
  getDiscoveryRequisites = asks envPeerDiscoveryRequisites
  {-# INLINE getDiscoveryRequisites #-}

instance HasShutdownFlag ServerM where
  getShutdownFlag = asks envShutdownFlag
  {-# INLINE getShutdownFlag #-}

instance MonadUnliftIO ServerM where
  askUnliftIO = ServerM $ (\(UnliftIO run) -> UnliftIO $ run . unServerM) <$> askUnliftIO
  withRunInIO go = ServerM $ withRunInIO (\k -> go $ k . unServerM)


instance MonadFees ServerM where
  getFees = do
    feeVar <- asks envFeeEstimates
    liftIO $ readTVarIO feeVar
  setFees cur fb = do
    feeVar <- asks envFeeEstimates
    liftIO $ atomically $ modifyTVar feeVar $ M.insert cur fb


instance HasConnectionsManagement ServerM where
  openConnections = asks envOpenConnections
  {-# INLINE openConnections #-}

instance HasBroadcastChannel ServerM where
  broadcastChannel = asks envBroadcastChannel
  {-# INLINE broadcastChannel #-}

stopThreadIfShutdown :: Thread -> ServerM ()
stopThreadIfShutdown thread = do
  shutdownFlag <- liftIO . readTVarIO =<< getShutdownFlag
  when shutdownFlag $ liftIO $ stop thread

broadcastSocketMessage :: Message -> ServerM ()
broadcastSocketMessage msg = liftIO . atomically . flip writeTChan msg =<< asks envBroadcastChannel
