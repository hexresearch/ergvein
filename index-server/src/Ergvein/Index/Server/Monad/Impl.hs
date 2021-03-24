{-# LANGUAGE DerivingVia #-}
module Ergvein.Index.Server.Monad.Impl
  (
    runServerMIO
  , runServerM
  , ServerM(..)
  -- Ergvein.Index.Server.Monad.Env re-exports
  , ServerEnv(..)
  , withNewServerEnv
  ) where

import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Prometheus (MonadMonitor(..))

import Ergvein.Index.Client
import Ergvein.Index.Server.Bitcoin.API
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.Monad.Class
import Ergvein.Index.Server.Monad.Env

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Network.Ergo.Api.Client     as ErgoApi

newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader ServerEnv, MonadThrow, MonadCatch, MonadMask, MonadBase IO)
  -- To avoid orphan we unwrap LoggingT as its reader representation
  deriving MonadMonitor via (ReaderT ServerEnv (ReaderT (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) IO))

newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT ServerEnv (LoggingT IO)) a }

instance MonadBaseControl IO ServerM where
  type StM ServerM a = StMServerM a
  liftBaseWith f = ServerM $ liftBaseWith $ \q -> f (fmap StMServerM . q . unServerM)
  restoreM = ServerM . restoreM . unStMServerM

runServerMIO :: ServerEnv -> ServerM a -> IO a
runServerMIO e = runChanLoggingT (envLogger e) . flip runReaderT e . unServerM

runServerM :: ServerEnv -> ServerM a -> LoggingT IO a
runServerM e = flip runReaderT e . unServerM

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

instance HasShutdownSignal ServerM where
  getShutdownFlag = asks envShutdownFlag
  getShutdownChannel = liftIO . atomically . dupTChan =<< asks envShutdownChannel
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

instance MonadRates ServerM where
  getRatesVar = asks envExchangeRates
  {-# INLINE getRatesVar #-}

instance HasThreadsManagement ServerM where
  registerThread parent child = do
    var <- asks envOpenThreads
    liftIO $ atomically $ modifyTVar var $ \m -> M.insertWith S.union parent (S.singleton child) m
  {-# INLINE registerThread #-}
  getThreadChildren tid = do
    var <- asks envOpenThreads
    m <- liftIO $ readTVarIO var
    pure $ maybe [] S.toList $ M.lookup tid m
  {-# INLINE getThreadChildren #-}

instance HasSocketsManagement ServerM where
  registerSocket saddr sock tid = do
    sockVar <- asks envOpenSockets
    liftIO $ atomically $ modifyTVar sockVar (M.insert saddr (sock, tid))
  {-# INLINE registerSocket #-}
  deregisterSocket saddr = do
    sockVar <- asks envOpenSockets
    liftIO $ atomically $ modifyTVar sockVar (M.delete saddr)
  {-# INLINE deregisterSocket #-}
  getManagedSocket saddr = do
    sockVar <- asks envOpenSockets
    m <- liftIO $ readTVarIO sockVar
    pure $ M.lookup saddr m
  {-# INLINE getManagedSocket #-}
  getAllManagedSockets = do
    sockVar <- asks envOpenSockets
    m <- liftIO $ readTVarIO sockVar
    pure $ M.keys m
  {-# INLINE getAllManagedSockets #-}

instance HasBroadcastChannel ServerM where
  broadcastChannel = asks envBroadcastChannel
  {-# INLINE broadcastChannel #-}

instance HasDbs ServerM where
  getDb = asks envDb
  {-# INLINE getDb #-}
  getUtxoCF _ = asks envBtcUtxoCF
  {-# INLINE getUtxoCF #-}
  getFiltersCF _ = asks envBtcFiltersCF
  {-# INLINE getFiltersCF #-}
  getMetaCF _ = asks envBtcMetaCF
  {-# INLINE getMetaCF #-}

instance HasBtcCache ServerM where
  getBtcCacheVar = asks envBtcCache
  {-# INLINE getBtcCacheVar #-}

instance LastScannedBlockStore ServerM where
  getLastScannedBlock _ = liftIO . readTVarIO =<< asks envBtcPrevHeader
  setLastScannedBlock _ val = do
    var <- asks envBtcPrevHeader
    liftIO . atomically $ writeTVar var val
  {-# INLINE getLastScannedBlock #-}
