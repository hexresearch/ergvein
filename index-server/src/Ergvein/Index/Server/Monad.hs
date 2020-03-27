module Ergvein.Index.Server.Monad where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Servant.Server
import Servant.Server.Generic
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.DB.Monad
import Control.Monad.IO.Unlift
import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import           Network.Ergo.Api.Client
import Ergvein.Index.Server.Config


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

instance BitcoinApiMonad ServerM where
  nodeRpcCall f = do 
    cfg <- asks $ envServerConfig

    liftIO $ BitcoinApi.withClient 
     (configBTCNodeHost     cfg)
     (configBTCNodePort     cfg)
     (configBTCNodeUser     cfg)
     (configBTCNodePassword cfg)
     f

instance HasBitcoinNodeNetwork ServerM where
  network = asks envBitconNodeNetwork

instance HasServerConfig ServerM where
  serverConfig = asks envServerConfig

instance MonadUnliftIO ServerM where
  askUnliftIO = ServerM $ (\(UnliftIO run) -> UnliftIO $ run . unServerM) <$> askUnliftIO
  withRunInIO go = ServerM $ withRunInIO (\k -> go $ k . unServerM)