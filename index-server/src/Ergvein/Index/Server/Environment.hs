
{-# OPTIONS_GHC -Wall #-}
module Ergvein.Index.Server.Environment where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Monad.Reader
import Network.HTTP.Client.TLS
import Network.Socket hiding (close)
import System.IO

import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Schema.Indexer (RollbackRecItem)
import Ergvein.Index.Server.DB.Worker
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.TCPService.BTC
import Ergvein.Types.Currency
import Ergvein.Types.Fees

import qualified Data.Map.Strict             as M
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Ergo.Api.Client     as ErgoApi
import qualified Network.Haskoin.Constants   as HK
import qualified Network.HTTP.Client         as HC

import Database.SQLite.Simple

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
    , envBtcRollback              :: !(TVar (Seq.Seq RollbackRecItem))
    , envPeerDiscoveryRequisites  :: !PeerDiscoveryRequisites
    , envFeeEstimates             :: !(TVar (M.Map CurrencyCode FeeBundle))
    , envShutdownFlag             :: !(TVar Bool)
    , envShutdownChannel          :: !(TChan Bool)
    , envOpenConnections          :: !(TVar (M.Map SockAddr (ThreadId, Socket)))
    , envBroadcastChannel         :: !(TChan Message)
    , envExchangeRates            :: !(TVar (M.Map CurrencyCode (M.Map Fiat Double)))
    -- DB
    , envFiltersDb                :: !Connection
    , envUtxoDb                   :: !Connection
    , envRollbackDb               :: !Connection
    , envDBCounter                :: !(TVar (Int,Int))
    , envCommitChannel            :: !(TChan BlockInfo)
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

withNewServerEnv :: (MonadIO m, MonadLogger m, MonadMask m, MonadBaseControl IO m)
  => Bool               -- ^ flag, def True: wait for node connections to be up before finalizing the env
  -> BitcoinApi.Client  -- ^ RPC connection to the bitcoin node
  -> Config             -- ^ Contents of the config file
  -> (ServerEnv -> m a)
  -> m a
withNewServerEnv useTcp btcClient cfg@Config{..} action = do
    logger <- liftIO newChan
    liftIO $ hSetBuffering stdout LineBuffering
    void $ liftIO $ forkIO $ runStdoutLoggingT $ unChanLoggingT logger
    ergoNodeClient <- liftIO $ ErgoApi.newClient cfgERGONodeHost cfgERGONodePort
    tlsManager     <- liftIO $ newTlsManager
    feeEstimates   <- liftIO $ newTVarIO M.empty
    shutdownVar    <- liftIO $ newTVarIO False
    openConns      <- liftIO $ newTVarIO M.empty
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


    -- DB ----------------------------
    -- fcon <- initConn DBFilters "db/filters.db"
    -- ucon <- initConn DBUtxo ":memory:"
    -- tcon <- initConn DBTxIndex "db/txs.db"


    (flitDb, utxoDb, rollDb, cntRef) <- initDbs cfgFiltersDbPath cfgUtxoDbPath "db/rollback.db"

    -- (conmem, con)   <- initUtxoConnection cfgUtxoDbPath
    -- rcon    <- initRollbackConn "db/rollback.db"
    -- filtsDb <- liftIO $ open cfgFiltersDbPath
    -- liftIO $ initBlockInfoRecTable filtsDb
    -- liftIO $ execute_ rcon [qc|attach '{cfgUtxoDbPath}' as udisk|]
    --
    -- cntRef <- liftIO $ newTVarIO (1,0)

    commitChan <- liftIO newTChanIO

    btcSeqVar <- liftIO $ newTVarIO mempty
    exchangeRates <- liftIO $ newTVarIO mempty
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
      , envBtcRollback             = btcSeqVar
      , envPeerDiscoveryRequisites = descDiscoveryRequisites
      , envFeeEstimates            = feeEstimates
      , envShutdownFlag            = shutdownVar
      , envShutdownChannel         = shutdownChan
      , envOpenConnections         = openConns
      , envBroadcastChannel        = broadChan
      , envExchangeRates           = exchangeRates
      , envFiltersDb               = flitDb
      , envUtxoDb                  = utxoDb
      , envRollbackDb              = rollDb
      , envCommitChannel           = commitChan
      , envDBCounter               = cntRef
      }
