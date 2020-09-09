{-# OPTIONS_GHC -Wall #-}
module Ergvein.Index.Server.Environment where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException(..),AsyncException(..))
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Typeable
import Database.LevelDB.Base
import Network.HTTP.Client.TLS
import Network.Socket

import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.TCPService.BTC
import Ergvein.Text
import Ergvein.Types.Fees

import qualified Data.Map.Strict             as M
import qualified Data.Set                    as Set
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi
import qualified Network.Haskoin.Constants   as HK
import qualified Network.HTTP.Client         as HC

import Debug.Trace

data ServerEnv = ServerEnv
    { envServerConfig             :: !Config
    , envLogger                   :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , envFiltersDBContext         :: !DB
    , envIndexerDBContext         :: !DB
    , envBitcoinNodeNetwork       :: !HK.Network
    , envErgoNodeClient           :: !ErgoApi.Client
    , envClientManager            :: HC.Manager
    , envBitcoinClient            :: !BitcoinApi.Client
    , envBitcoinSocket            :: !BtcSocket
    , envPeerDiscoveryRequisites  :: !PeerDiscoveryRequisites
    , envFeeEstimates             :: !(TVar (M.Map CurrencyCode FeeBundle))
    , envShutdownFlag             :: !(TVar Bool)
    , envOpenConnections          :: !(TVar (M.Map SockAddr (ThreadId, Socket)))
    , envBroadcastChannel         :: !(TChan Message)
    }

discoveryRequisites :: Config -> PeerDiscoveryRequisites
discoveryRequisites cfg = let
  ownPeerAddress = SockAddrUnix <$> cfgOwnPeerAddress cfg
  knownPeers = Set.fromList $ SockAddrUnix <$> cfgKnownPeers cfg
  filteredKnownPeers = case ownPeerAddress of
    Just address -> Set.delete address knownPeers
    _    -> knownPeers
  in PeerDiscoveryRequisites
      ownPeerAddress
      filteredKnownPeers
      (cfgPeerActualizationDelay cfg)
      (cfgPeerActualizationTimeout cfg)

newServerEnv :: (MonadIO m, MonadLogger m, MonadMask m, MonadBaseControl IO m)
  => Bool               -- ^ flag, def True: wait for node connections to be up before finalizing the env
  -> Bool               -- ^ flag, def False: do not drop Filters database
  -> BitcoinApi.Client  -- ^ RPC connection to the bitcoin node
  -> Config             -- ^ Contents of the config file
  -> m ServerEnv
newServerEnv doWait noDropFilters btcClient cfg@Config{..} = do
    logger <- liftIO newChan
    void $ liftIO $ forkIO $ runStdoutLoggingT $ unChanLoggingT logger
    filtersDBCntx  <- openDb noDropFilters DBFilters cfgFiltersDbPath
    indexerDBCntx  <- openDb noDropFilters DBIndexer cfgIndexerDbPath
    ergoNodeClient <- liftIO $ ErgoApi.newClient cfgERGONodeHost cfgERGONodePort
    tlsManager     <- liftIO $ newTlsManager
    feeEstimates   <- liftIO $ newTVarIO M.empty
    shutdownVar    <- liftIO $ newTVarIO False
    openConns      <- liftIO $ newTVarIO M.empty
    broadChan      <- liftIO newBroadcastTChanIO
    let bitcoinNodeNetwork = if cfgBTCNodeIsTestnet then HK.btcTest else HK.btc
        descDiscoveryRequisites = discoveryRequisites cfg
    btcsock <- connectBtc bitcoinNodeNetwork cfgBTCNodeTCPHost (show cfgBTCNodeTCPPort) shutdownVar
    when doWait $ liftIO $ do
      isUp <- atomically $ dupTChan $ btcSockOnActive btcsock
      b <- readTVarIO $ btcSockIsActive btcsock
      if b then pure () else fix $ \next -> do
        b' <- atomically $ readTChan isUp
        if b' then pure () else next
    traceShowM cfg
    pure ServerEnv
      { envServerConfig            = cfg
      , envLogger                  = logger
      , envFiltersDBContext        = filtersDBCntx
      , envIndexerDBContext        = indexerDBCntx
      , envBitcoinNodeNetwork      = bitcoinNodeNetwork
      , envErgoNodeClient          = ergoNodeClient
      , envClientManager           = tlsManager
      , envBitcoinClient           = btcClient
      , envBitcoinSocket           = btcsock
      , envPeerDiscoveryRequisites = descDiscoveryRequisites
      , envFeeEstimates            = feeEstimates
      , envShutdownFlag            = shutdownVar
      , envOpenConnections         = openConns
      , envBroadcastChannel        = broadChan
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
