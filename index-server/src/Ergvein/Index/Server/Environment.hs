module Ergvein.Index.Server.Environment where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException(..),AsyncException(..))
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.ByteString.UTF8
import Data.Maybe
import Data.Typeable
import Database.LevelDB.Base
import Network.Bitcoin.Api.Types
import Network.HTTP.Client.TLS
import Servant.Client.Core

import Ergvein.Index.Server.Cache
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees

import qualified Data.Map.Strict             as M
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi
import qualified Network.Haskoin.Constants   as HK
import qualified Network.HTTP.Client         as HC
import qualified Data.Set                    as Set

import Debug.Trace

data ServerEnv = ServerEnv
    { envServerConfig             :: !Config
    , envLogger                   :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , envLevelDBContext           :: !DB
    , envBitcoinNodeNetwork       :: !HK.Network
    , envErgoNodeClient           :: !ErgoApi.Client
    , envClientManager            :: !HC.Manager
    , envPeerDiscoveryRequisites  :: !PeerDiscoveryRequisites
    , envFeeEstimates             :: !(TVar (M.Map Currency FeeBundle))
    }

discoveryRequisites :: Config -> PeerDiscoveryRequisites
discoveryRequisites cfg = let
  ownPeerAddress = parsedOwnAddress <$> cfgOwnPeerAddress cfg
  knownPeers = Set.fromList $ parseKnownPeer <$> cfgKnownPeers cfg
  filteredKnownPeers = case ownPeerAddress of
    Just address -> Set.delete address knownPeers
    otherwise    -> knownPeers
  in PeerDiscoveryRequisites
      ownPeerAddress
      filteredKnownPeers
      (cfgPeerActualizationDelay cfg)
      (cfgPeerActualizationTimeout cfg)
  where

    parsedOwnAddress :: String -> BaseUrl
    parsedOwnAddress address = let
      err = error $ "Error cannot parse ownPeerAddress setting"
      in fromMaybe err $ parseBaseUrl address

    parseKnownPeer :: String -> BaseUrl
    parseKnownPeer address = let
      err = error $ "Error cannot parse peer '" <> address <> "'"
      in fromMaybe err $ parseBaseUrl address

newServerEnv :: (MonadIO m, MonadLogger m) => Config -> m ServerEnv
newServerEnv cfg = do
    logger <- liftIO newChan
    liftIO $ forkIO $ runStdoutLoggingT $ unChanLoggingT logger


    levelDBContext <- openCacheDb (cfgCachePath cfg)
    ergoNodeClient <- liftIO $ ErgoApi.newClient (cfgERGONodeHost cfg) (cfgERGONodePort cfg)
    httpManager    <- liftIO $ HC.newManager HC.defaultManagerSettings
    tlsManager     <- liftIO $ newTlsManager
    feeEstimates   <- liftIO $ newTVarIO M.empty
    let bitcoinNodeNetwork = if cfgBTCNodeIsTestnet cfg then HK.btcTest else HK.btc
        descDiscoveryRequisites = discoveryRequisites cfg
    traceShowM cfg
    pure ServerEnv
      { envServerConfig            = cfg
      , envLogger                  = logger
      , envLevelDBContext          = levelDBContext
      , envBitcoinNodeNetwork      = bitcoinNodeNetwork
      , envErgoNodeClient          = ergoNodeClient
      , envClientManager           = tlsManager
      , envPeerDiscoveryRequisites = descDiscoveryRequisites
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
