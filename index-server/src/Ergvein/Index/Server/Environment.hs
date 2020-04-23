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
import Network.HTTP.Client.TLS
import Network.Connection

import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi
import qualified Network.Haskoin.Constants   as HK
import qualified Network.HTTP.Client as HC


import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (newTlsManagerWith, mkManagerSettings, newTlsManager)
import Network.TLS
import Network.TLS.Extra.Cipher

data ServerEnv = ServerEnv 
    { envServerConfig      :: !Config
    , envLogger            :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , envPersistencePool   :: !DBPool
    , envLevelDBContext    :: !DB
    , envBitconNodeNetwork :: !HK.Network
    , envErgoNodeClient    :: !ErgoApi.Client
    , envHttpClient        :: HC.Manager
    }

class HasBitcoinNodeNetwork m where
  currentBitcoinNetwork :: m HK.Network

newServerEnv :: (MonadIO m, MonadLogger m) => Config -> m ServerEnv
newServerEnv cfg = do
    logger <- liftIO newChan
    void . liftIO . forkIO $ runStdoutLoggingT $ unChanLoggingT logger
    persistencePool <- liftIO $ runStdoutLoggingT $ do
        let dbLog = configDbLog cfg
        persistencePool <- newDBPool dbLog $ fromString $ connectionStringFromConfig cfg
        flip runReaderT persistencePool $ dbQuery $ runMigration migrateAll
        pure persistencePool
    levelDBContext <- openCacheDb (configCachePath cfg) persistencePool
    ergoNodeClient <- liftIO $ ErgoApi.newClient (configERGONodeHost cfg) $ (configERGONodePort cfg)
    let bitconNodeNetwork = if configBTCNodeIsTestnet cfg then HK.btcTest else HK.btc
    
    manager <- liftIO $ newTlsManager

    pure ServerEnv { envServerConfig    = cfg
                   , envLogger          = logger
                   , envPersistencePool = persistencePool
                   , envLevelDBContext  = levelDBContext
                   , envBitconNodeNetwork = bitconNodeNetwork
                   , envErgoNodeClient  = ergoNodeClient
                   , envHttpClient = manager
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
