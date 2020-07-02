module Ergvein.Index.Server.Monad where

import Control.Concurrent.STM
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Servant.Server
import Servant.Server.Generic

import Ergvein.Index.Client
import Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency
import Ergvein.Types.Fees

import qualified Data.Map.Strict as M
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi

newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader ServerEnv, MonadThrow, MonadCatch, MonadMask)

type AsServerM = AsServerT ServerM

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

instance MonadLDB ServerM where
  getDb = asks envLevelDBContext
  {-# INLINE getDb #-}

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
  nodeRpcCall f = do
    cfg <- asks $ envServerConfig

    liftIO $ BitcoinApi.withClient
     (cfgBTCNodeHost     cfg)
     (cfgBTCNodePort     cfg)
     (cfgBTCNodeUser     cfg)
     (cfgBTCNodePassword cfg)
     f

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
  getFees :: m (M.Map Currency FeeBundle)
  setFees :: Currency -> FeeBundle -> m ()

instance MonadFees ServerM where
  getFees = do
    feeVar <- asks envFeeEstimates
    liftIO $ readTVarIO feeVar
  setFees cur fb = do
    feeVar <- asks envFeeEstimates
    liftIO $ atomically $ modifyTVar feeVar $ M.insert cur fb
