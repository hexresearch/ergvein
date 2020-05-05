module Ergvein.Index.Server.Monad where

import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.Environment
import Network.Ergo.Api.Client
import Servant.Server
import Servant.Server.Generic
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import Ergvein.Index.Client
import Control.Concurrent.MVar

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

instance MonadDB ServerM where
  getDbPool = asks envPersistencePool
  {-# INLINE getDbPool #-}

instance MonadLDB ServerM where
  getDb = asks envLevelDBContext
  {-# INLINE getDb #-}

instance ApiMonad ServerM where
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
     (configBTCNodeHost     cfg)
     (configBTCNodePort     cfg)
     (configBTCNodeUser     cfg)
     (configBTCNodePassword cfg)
     f

instance HasClientManager ServerM where
  getClientManager = asks envHttpManager
  {-# INLINE getClientManager #-}

instance HasHttpManager ServerM where
  getHttpManager = asks envHttpManager
  {-# INLINE getHttpManager #-}

instance HasTlsManager ServerM where
  getTlsManager = asks envTlsManager
  {-# INLINE getTlsManager #-}

instance HasDiscoveryRequisites ServerM where
  getDiscoveryRequisites = asks envPeerDiscoveryRequisites
  {-# INLINE getDiscoveryRequisites #-}

instance MonadUnliftIO ServerM where
  askUnliftIO = ServerM $ (\(UnliftIO run) -> UnliftIO $ run . unServerM) <$> askUnliftIO
  withRunInIO go = ServerM $ withRunInIO (\k -> go $ k . unServerM)