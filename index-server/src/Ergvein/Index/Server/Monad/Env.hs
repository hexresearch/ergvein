module Ergvein.Index.Server.Monad.Env
  (
    withNewServerEnv
  , ServerEnv(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Fixed
import Data.Map.Strict (Map)
import Data.Set (Set)
import Database.RocksDB (withDBCF, DB(..), ColumnFamily)
import Network.HTTP.Client.TLS
import Network.Socket
import System.IO

import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.Bitcoin.API
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Socket.BTC
import Ergvein.Types.Currency
import Ergvein.Types.Fees

import qualified Data.Map.Strict             as M
import qualified Data.Set                    as Set
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi
import qualified Network.Haskoin.Constants   as HK
import qualified Network.HTTP.Client         as HC

data ServerEnv = ServerEnv
    { envServerConfig             :: !Config
    , envLogger                   :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , envBitcoinNodeNetwork       :: !HK.Network
    , envErgoNodeClient           :: !ErgoApi.Client
    , envClientManager            :: !HC.Manager
    , envBitcoinClient            :: !BitcoinApi.Client
    , envBitcoinSocket            :: !BtcSocket
    , envBitcoinSocketReconnect   :: !(IO ())
    , envBtcConScheme             :: !(TVar BtcConnectionScheme)
    , envPeerDiscoveryRequisites  :: !PeerDiscoveryRequisites
    , envFeeEstimates             :: !(TVar (Map CurrencyCode FeeBundle))
    , envShutdownFlag             :: !(TVar Bool)
    , envShutdownChannel          :: !(TChan Bool)
    , envOpenSockets              :: !(TVar (Map SockAddr (Socket, ThreadId)))
    , envOpenThreads              :: !(TVar (Map ThreadId (Set ThreadId)))
    , envBroadcastChannel         :: !(TChan Message)
    , envExchangeRates            :: !(TVar (Map CurrencyCode (Map Fiat Centi)))
    -- DB
    -- Currency we store columnFamilies for BTC only
    -- Thus they are explicitly enumerated here
    , envDb                       :: !DB
    , envBtcUtxoCF                :: !ColumnFamily
    , envBtcFiltersCF             :: !ColumnFamily
    , envBtcMetaCF                :: !ColumnFamily
    }

sockAddress :: CfgPeer -> IO SockAddr
sockAddress CfgPeer {..} = do
  addr:_ <- getAddrInfo (Just hints) (Just cfgPeerIP) (Just cfgPeerPort)
  pure $ addrAddress addr
  where
    hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            , addrFamily = AF_INET
            , addrProtocol = 0
            }

discoveryRequisites :: Config -> IO PeerDiscoveryRequisites
discoveryRequisites cfg = do
  ownPeerAddress <- sequence $ sockAddress <$> cfgOwnPeerAddress cfg
  knownPeers <- Set.fromList <$> (mapM sockAddress $ cfgKnownPeers cfg)
  let filteredKnownPeers = case ownPeerAddress of
                             Just address -> Set.delete address knownPeers
                             _    -> knownPeers
  pure $ PeerDiscoveryRequisites
      ownPeerAddress
      filteredKnownPeers
      (cfgPeerActualizationDelay cfg)
      (cfgPeerActualizationTimeout cfg)

withNewServerEnv :: (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadMask m, MonadBaseControl IO m)
  => Bool               -- ^ flag, def True: wait for node connections to be up before finalizing the env
  -> BitcoinApi.Client  -- ^ RPC connection to the bitcoin node
  -> Config             -- ^ Contents of the config file
  -> (ServerEnv -> m a)
  -> m a
withNewServerEnv useTcp btcClient cfg@Config{..} action =
  withDBCF cfgFiltersDbPath dbConfig dbColumns $ \db -> do
    logger <- liftIO newChan
    liftIO $ hSetBuffering stdout LineBuffering
    void $ liftIO $ forkIO $ runStdoutLoggingT $ unChanLoggingT logger
    ergoNodeClient <- liftIO $ ErgoApi.newClient cfgERGONodeHost cfgERGONodePort
    tlsManager     <- liftIO $ newTlsManager
    feeEstimates   <- liftIO $ newTVarIO M.empty
    shutdownVar    <- liftIO $ newTVarIO False
    openSockets    <- liftIO $ newTVarIO M.empty
    openThreads    <- liftIO $ newTVarIO M.empty
    broadChan      <- liftIO newBroadcastTChanIO
    shutdownChan   <- liftIO newTChanIO
    btcRestartChan <- liftIO newTChanIO
    btcConnVar     <- liftIO $ newTVarIO $ if useTcp then BtcConTCP else BtcConRPC
    let bitcoinNodeNetwork = if cfgBTCNodeIsTestnet then HK.btcTest else HK.btc
    descDiscoveryRequisites <- liftIO $ discoveryRequisites cfg
    btcsock <- if useTcp then do
      btcsock <- connectBtc
        bitcoinNodeNetwork
        cfgBTCNodeTCPHost
        (show cfgBTCNodeTCPPort)
        shutdownVar
        btcRestartChan
      liftIO $ do
        isUp <- atomically $ dupTChan $ btcSockOnActive btcsock
        b <- readTVarIO $ btcSockIsActive btcsock
        unless b $ fix $ \next -> do
          b' <- atomically $ readTChan isUp
          unless b' next
      pure btcsock
      else dummyBtcSock bitcoinNodeNetwork
    exchangeRates <- liftIO $ newTVarIO mempty
    let [ucf, fcf, mcf] = columnFamilies db
    action ServerEnv
      { envServerConfig            = cfg
      , envLogger                  = logger
      , envBitcoinNodeNetwork      = bitcoinNodeNetwork
      , envErgoNodeClient          = ergoNodeClient
      , envClientManager           = tlsManager
      , envBitcoinClient           = btcClient
      , envBitcoinSocket           = btcsock
      , envBitcoinSocketReconnect  = liftIO (atomically $ writeTChan btcRestartChan ())
      , envBtcConScheme            = btcConnVar
      , envPeerDiscoveryRequisites = descDiscoveryRequisites
      , envFeeEstimates            = feeEstimates
      , envShutdownFlag            = shutdownVar
      , envShutdownChannel         = shutdownChan
      , envOpenSockets             = openSockets
      , envOpenThreads             = openThreads
      , envBroadcastChannel        = broadChan
      , envExchangeRates           = exchangeRates
      , envDb                      = db
      , envBtcUtxoCF               = ucf
      , envBtcFiltersCF            = fcf
      , envBtcMetaCF               = mcf
      }
