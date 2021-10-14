
{-# OPTIONS_GHC -Wall #-}
module Ergvein.Index.Server.Environment where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text, isInfixOf)
import Data.Typeable
import Data.Fixed
import Network.HTTP.Client.TLS
import Network.Socket
import System.IO

import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema.Indexer (RollbackRecItem, RollbackSequence(..))
import Ergvein.Index.Server.DB.Wrapper
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.TCPService.BTC
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees

import qualified Data.Map.Strict             as M
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Haskoin.Constants   as HK
import qualified Network.HTTP.Client         as HC

data ServerEnv = ServerEnv
    { envServerConfig             :: !Config
    , envLogger                   :: !(Chan (Loc, LogSource, LogLevel, LogStr))
    , envFiltersDBContext         :: !LevelDB
    , envIndexerDBContext         :: !LevelDB
    , envUtxoDBContext            :: !LevelDB
    , envBitcoinNodeNetwork       :: !HK.Network
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
    , envExchangeRates            :: !(TVar (M.Map CurrencyCode (M.Map Fiat Centi)))
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
  -> Bool               -- ^ flag, def False: do not drop Filters database
  -> Bool               -- ^ flag, def False: do not drop Indexer's database
  -> Bool               -- ^ flag, def False: do not drop Utxo's database
  -> BitcoinApi.Client  -- ^ RPC connection to the bitcoin node
  -> Config             -- ^ Contents of the config file
  -> (ServerEnv -> m a)
  -> m a
withNewServerEnv useTcp overrideFilters overridesIndexers overridesUtxo btcClient cfg@Config{..} action = do
    logger <- liftIO newChan
    liftIO $ hSetBuffering stdout LineBuffering
    void $ liftIO $ forkIO $ runStdoutLoggingT $ unChanLoggingT logger
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
    withDb overrideFilters DBFilters cfgFiltersDbPath $ \filtersDB -> do
      withDb overridesIndexers DBIndexer cfgIndexerDbPath $ \indexerDB -> do
        withDb overridesUtxo DBUtxo cfgUtxoDbPath $ \utxoDB -> do
          btcSeq    <- liftIO $ runStdoutLoggingT $ runReaderT (loadRollbackSequence BTC) indexerDB
          btcSeqVar <- liftIO $ newTVarIO $ unRollbackSequence btcSeq
          exchangeRates <- liftIO $ newTVarIO mempty
          action ServerEnv
            { envServerConfig            = cfg
            , envLogger                  = logger
            , envFiltersDBContext        = filtersDB
            , envIndexerDBContext        = indexerDB
            , envUtxoDBContext           = utxoDB
            , envBitcoinNodeNetwork      = bitcoinNodeNetwork
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
            }

-- | Log exceptions at Error severity
logOnException :: (
    HasServerConfig m
  , HasFiltersDB m
  , HasIndexerDB m
  , HasUtxoDB m
  , MonadIO m
  , MonadLogger m
  , MonadCatch m)
  => Text -> m a -> m a
logOnException threadName = handle logE
  where
    logInfoN' :: MonadLogger m => Text -> m ()
    logInfoN' t = logInfoN $ "[" <> threadName <> "]: " <> t
    logErrorN' :: MonadLogger m => Text -> m ()
    logErrorN' t = logErrorN $ "[" <> threadName <> "]: " <> t
    logE :: (
        HasServerConfig m
      , HasFiltersDB m
      , HasIndexerDB m
      , HasUtxoDB m
      , MonadIO m
      , MonadLogger m
      , MonadCatch m) => SomeException -> m b
    logE e
        | Just ThreadKilled <- fromException e = do
            logInfoN' "Killed normally by ThreadKilled"
            throwM e
        | Just (ioe :: IOException) <- fromException e = do
          let isBadMagic = isInfixOf "not an sstable" $ showt ioe
          if isBadMagic
            then do
              logInfoN' "Halted by \"not an sstable (bad magic nuber)\" error. Repairing the db."
              Config{..} <- serverConfig
              fdb <- getFiltersDb
              idb <- getIndexerDb
              udb <- getUtxoDb
              reopenLevelDB fdb
              reopenLevelDB idb
              reopenLevelDB udb
              logInfoN' "Reopened the db. Resume as usual"
            else
              logErrorN' $ "Killed by IOException. " <> showt ioe
          liftIO $ threadDelay 1000000
          throwM e
        | SomeException eTy <- e = do
            logErrorN' $ "Killed by " <> showt (typeOf eTy) <> ". " <> showt eTy
            liftIO $ threadDelay 1000000
            throwM e
