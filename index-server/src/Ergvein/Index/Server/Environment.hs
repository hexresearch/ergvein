module Ergvein.Index.Server.Environment where

import Control.Concurrent
import Control.Exception (SomeException(..),AsyncException(..))
import Control.Monad.Catch
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
import Control.Monad.IO.Unlift

import Ergvein.Index.Server.Cache
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Text

import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi
import qualified Network.Haskoin.Constants   as HK
import qualified Network.HTTP.Client         as HC

data ServerEnv = ServerEnv 
    { envServerConfig             :: !Config
    , envLogger                   :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , envPersistencePool          :: !DBPool
    , envLevelDBContext           :: !DB
    , envBitcoinNodeNetwork       :: !HK.Network
    , envErgoNodeClient           :: !ErgoApi.Client
    , envHttpManager              :: !HC.Manager
    , envTlsManager               :: !HC.Manager
    , envPeerDiscoveryRequisites  :: !PeerDiscoveryRequisites
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
        persistencePool <- getPersistencePool (configDbLog cfg) (connectionStringFromConfig cfg)
        applyDatabaseMigration persistencePool
        pure persistencePool

    levelDBContext <- openCacheDb (configCachePath cfg) persistencePool
    ergoNodeClient <- liftIO $ ErgoApi.newClient (configERGONodeHost cfg) (configERGONodePort cfg)
    httpManager    <- liftIO $ HC.newManager HC.defaultManagerSettings
    tlsManager     <- liftIO $ newTlsManager

    let bitcoinNodeNetwork = if configBTCNodeIsTestnet cfg then HK.btcTest else HK.btc
        peerDiscoveryRequisites = PeerDiscoveryRequisites 
                                  (fromJust $ parseBaseUrl $ configOwnPeerAddress cfg)
                                  (fromJust . parseBaseUrl <$> configKnownPeers cfg)
    pure ServerEnv 
      { envServerConfig            = cfg
      , envLogger                  = logger
      , envPersistencePool         = persistencePool
      , envLevelDBContext          = levelDBContext
      , envBitcoinNodeNetwork      = bitcoinNodeNetwork
      , envErgoNodeClient          = ergoNodeClient
      , envHttpManager             = httpManager
      , envTlsManager              = tlsManager
      , envPeerDiscoveryRequisites = peerDiscoveryRequisites
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
