module Ergvein.Index.Server.Environment where

import Control.Concurrent
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.UTF8
import Database.Persist.Sql
import Network.Bitcoin.Api.Client
import Network.Bitcoin.Api.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.BlockchainCache
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Database.LevelDB
import Data.Default
import Database.LevelDB.Higher

data ServerEnv = ServerEnv 
    { envConfig :: !Config
    , envLogger :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , envPool   :: !DBPool
    }

btcNodeClient :: Config -> (Client -> IO a) -> IO a
btcNodeClient cfg = withClient 
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
    storedInfos <- liftIO $ fromPersisted pool
    liftIO $ runCreateLevelDB "/tmp/mydb" "txOuts" $ do
        deleteHistory
        addToCache storedInfos
    pure ServerEnv { envConfig = cfg
                   , envLogger = logger
                   , envPool   = pool
                   }