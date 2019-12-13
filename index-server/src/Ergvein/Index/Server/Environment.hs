module Ergvein.Index.Server.Environment where

import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.UTF8
import Database.LevelDB.Base
import Database.Persist.Sql

import Ergvein.Index.Server.Cache
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema

import qualified Network.Bitcoin.Api.Client as BitcoinApi
import qualified Network.Ergo.Api.Client as ErgoApi

data ServerEnv = ServerEnv 
    { env'config            :: !Config
    , env'Logger            :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , env'persistencePool   :: !DBPool
    , env'levelDBContext    :: !DB
    , env'ergoNodeClient    :: !ErgoApi.Client
    }

btcNodeClient :: Config -> (BitcoinApi.Client -> IO a) -> IO a
btcNodeClient cfg = BitcoinApi.withClient 
    (configBTCNodeHost     cfg)
    (configBTCNodePort     cfg)
    (configBTCNodeUser     cfg)
    (configBTCNodePassword cfg)

newServerEnv :: MonadIO m => Config -> m ServerEnv
newServerEnv cfg = do
    logger <- liftIO newChan
    pool <- liftIO $ runStdoutLoggingT $ do
        pool <- newDBPool $ fromString $ connectionStringFromConfig cfg
        flip runReaderT pool $ runDb $ runMigration migrateAll
        pure pool
    levelDBContext <- liftIO $ openDb
    liftIO $ runDbQuery pool $ loadCache levelDBContext
    ergoNodeClient <- liftIO $ ErgoApi.newClient (configERGONodeHost cfg) $ (configERGONodePort cfg)
    pure ServerEnv { env'config          = cfg
                   , env'Logger          = logger
                   , env'persistencePool = pool
                   , env'levelDBContext  = levelDBContext
                   , env'ergoNodeClient  = ergoNodeClient
                   }