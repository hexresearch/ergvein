module Ergvein.Index.Server.Monad where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Network.Socket
import Servant.Server
import Servant.Server.Generic

import Ergvein.Index.Client
import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency
import Ergvein.Types.Fees

import qualified Data.Map.Strict as M
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi

newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader ServerEnv, MonadThrow, MonadCatch, MonadMask, MonadBase IO)

type AsServerM = AsServerT ServerM

newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT ServerEnv (LoggingT IO)) a }

instance MonadBaseControl IO ServerM where
  type StM ServerM a = StMServerM a
  liftBaseWith f = ServerM $ liftBaseWith $ \q -> f (fmap StMServerM . q . unServerM)
  restoreM = ServerM . restoreM . unStMServerM

catchHandler :: IO a -> Handler a
catchHandler = Handler . ExceptT . try

runServerM :: ServerEnv -> ServerM a -> Handler a
runServerM e = catchHandler . runChanLoggingT (envLogger e) . flip runReaderT e . unServerM

runServerMIO :: ServerEnv -> ServerM a -> IO a
runServerMIO env m = do
  ea <- runHandler $ runServerM env m
  case ea of
    Left e -> fail $ "runServerMIO: " <> show e
    Right a -> return a

instance HasFiltersDB ServerM where
  getFiltersDb = asks envFiltersDBContext
  {-# INLINE getFiltersDb #-}

instance HasIndexerDB ServerM where
  getIndexerDb = asks envIndexerDBContext
  {-# INLINE getIndexerDb #-}

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

-- Fee functionality
class MonadFees m where
  getFees :: m (M.Map CurrencyCode FeeBundle)
  setFees :: CurrencyCode -> FeeBundle -> m ()

instance MonadFees ServerM where
  getFees = do
    feeVar <- asks envFeeEstimates
    liftIO $ readTVarIO feeVar
  setFees cur fb = do
    feeVar <- asks envFeeEstimates
    liftIO $ atomically $ modifyTVar feeVar $ M.insert cur fb


stopThreadIfShutdown :: Thread -> ServerM ()
stopThreadIfShutdown thread = do
  shutdownFlag <- liftIO . readTVarIO =<< getShutdownFlag
  when shutdownFlag $ liftIO $ stop thread

broadcastSocketMessage :: Message -> ServerM ()
broadcastSocketMessage msg = liftIO . atomically . flip writeTChan msg =<< asks envBroadcastChannel

closeConnection :: (ThreadId, Socket) -> IO ()
closeConnection (connectionThreadId, connectionSocket) = close connectionSocket >> killThread connectionThreadId

closePeerConnection :: SockAddr -> ServerM ()
closePeerConnection addr = do
  openedConnectionsRef <- asks envOpenConnections
  liftIO $ closeConnection =<< (M.! addr) <$> readTVarIO openedConnectionsRef
