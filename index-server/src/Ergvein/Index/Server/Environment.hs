module Ergvein.Index.Server.Environment where

import Control.Concurrent
import Control.Exception (SomeException(..),AsyncException(..))
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.UTF8
import Database.LevelDB.Base
import Data.Default
import Data.Typeable
import Database.LevelDB.Base
import Database.Persist.Sql

import Ergvein.Index.Server.Cache
import Ergvein.Index.Server.Cache
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Text
import Network.Bitcoin.Api.Client
import Network.Bitcoin.Api.Types
import Network.Bitcoin.Api.Misc

import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi
import qualified Network.Haskoin.Constants   as HK

data ServerEnv = ServerEnv 
    { envServerConfig      :: !Config
    , envLogger            :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , envPersistencePool   :: !DBPool
    , envLevelDBContext    :: !DB
    , envBitconNodeNetwork :: !HK.Network
    , envErgoNodeClient    :: !ErgoApi.Client
    }

btcNodeClient :: Config -> (BitcoinApi.Client -> IO a) -> IO a
btcNodeClient cfg = BitcoinApi.withClient 
    (configBTCNodeHost     cfg)
    (configBTCNodePort     cfg)
    (configBTCNodeUser     cfg)
    (configBTCNodePassword cfg)

newServerEnv :: (MonadIO m, MonadLogger m) => Config -> m ServerEnv
newServerEnv cfg = do
    logger <- liftIO newChan
    pool   <- liftIO $ runStdoutLoggingT $ do
        let doLog = configDbLog cfg
        pool <- newDBPool doLog $ fromString $ connectionStringFromConfig cfg
        flip runReaderT pool $ runDb $ runMigration migrateAll
        pure pool
    levelDBContext <- liftIO $ openDb
    loadCache levelDBContext pool
    ergoNodeClient <- liftIO $ ErgoApi.newClient (configERGONodeHost cfg) $ (configERGONodePort cfg)
    let bitconNodeNetwork = if configBTCNodeIsTestnet cfg then HK.btcTest else HK.btc
    pure ServerEnv { envServerConfig    = cfg
                   , envLogger          = logger
                   , envPersistencePool = pool
                   , envLevelDBContext  = levelDBContext
                   , envBitconNodeNetwork = bitconNodeNetwork
                   , envErgoNodeClient  = ergoNodeClient
                   }

-- | Log exceptions at Error severity
logOnException :: (MonadIO m, MonadLogger m, MonadCatch m) => m a -> m a
logOnException = handle logE
  where
    logE e
        | Just ThreadKilled <- fromException e = do
            logInfoN "Killed normally by ThreadKilled"
            throwM e
        | SomeException eTy <- e = do
            logErrorN $ "Killed by " <> showt (typeOf eTy) <> showt eTy
            liftIO $ threadDelay 1000000
            throwM e
