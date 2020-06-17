module Ergvein.Index.Server.Environment where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException(..),AsyncException(..))
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.UTF8
import Data.Maybe
import Data.Typeable
import Database.LevelDB.Base
import Database.Persist.Sql
import Network.Bitcoin.Api.Types
import Network.HTTP.Client.TLS
import Servant.Client.Core

import Ergvein.Index.Server.Cache
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees

import qualified Data.Map.Strict as M
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi
import qualified Network.Haskoin.Constants   as HK
import qualified Network.HTTP.Client         as HC
import qualified Data.Set as Set

import Debug.Trace

data ServerEnv = ServerEnv
    { envServerConfig             :: !Config
    , envLogger                   :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , envPersistencePool          :: !DBPool
    , envLevelDBContext           :: !DB
    , envBitcoinNodeNetwork       :: !HK.Network
    , envErgoNodeClient           :: !ErgoApi.Client
    , envClientManager            :: !HC.Manager
    , envPeerDiscoveryRequisites  :: !PeerDiscoveryRequisites
    , envFeeEstimates             :: !(TVar (M.Map Currency FeeBundle))
    }

getPersistencePool :: MonadUnliftIO m => Bool -> String ->  LoggingT m DBPool
getPersistencePool isLogEnabled connectionString = newDBPool isLogEnabled $ fromString connectionString

applyDatabaseMigration :: DBPool -> LoggingT IO ()
applyDatabaseMigration persistencePool = flip runReaderT persistencePool $ dbQuery $ runMigration migrateAll

newServerEnv :: (MonadIO m, MonadLogger m) => Config -> m ServerEnv
newServerEnv cfg = do
    logger <- liftIO newChan
    liftIO $ forkIO $ runStdoutLoggingT $ unChanLoggingT logger

    persistencePool <- liftIO $ runStdoutLoggingT $ do
        persistencePool <- getPersistencePool (cfgDbLog cfg) (connectionStringFromConfig cfg)
        applyDatabaseMigration persistencePool
        pure persistencePool

    levelDBContext <- openCacheDb (cfgCachePath cfg) persistencePool
    ergoNodeClient <- liftIO $ ErgoApi.newClient (cfgERGONodeHost cfg) (cfgERGONodePort cfg)
    httpManager    <- liftIO $ HC.newManager HC.defaultManagerSettings
    tlsManager     <- liftIO $ newTlsManager
    feeEstimates   <- liftIO $ newTVarIO M.empty
    let bitcoinNodeNetwork = if cfgBTCNodeIsTestnet cfg then HK.btcTest else HK.btc
        descReqoveryRequisites = PeerDiscoveryRequisites
                                  (parseBaseUrl @Maybe <=< cfgOwnPeerAddress $ cfg)
                                  (Set.fromList $ fromJust . parseBaseUrl <$> cfgKnownPeers cfg)
                                  (cfgPeerActualizationDelay cfg)
                                  (cfgPeerActualizationTimeout cfg)
    traceShowM cfg
    pure ServerEnv
      { envServerConfig            = cfg
      , envLogger                  = logger
      , envPersistencePool         = persistencePool
      , envLevelDBContext          = levelDBContext
      , envBitcoinNodeNetwork      = bitcoinNodeNetwork
      , envErgoNodeClient          = ergoNodeClient
      , envClientManager           = tlsManager
      , envPeerDiscoveryRequisites = descReqoveryRequisites
      , envFeeEstimates            = feeEstimates
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
