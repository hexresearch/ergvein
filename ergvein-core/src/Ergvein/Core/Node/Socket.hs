module Ergvein.Core.Node.Socket(
    InboundException
  , ConnectException
  , CloseException
  , ReceiveException(..)
  , MonadPeeker(..)
  , Peer(..)
  , N.ServiceName
  , N.HostName
  , SocketConf(..)
  , SocketStatus(..)
  , CloseReason(..)
  , isCloseFinal
  , Socket(..)
  , socketConnected
  , noSocket
  , switchSocket
  , socket
  , N.SockAddr
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString (ByteString)
import GHC.Generics
import Network.Socks5 (SocksConf(..), socksConnect, SocksAddress(..), SocksHostAddress(..))
import Reflex
import Reflex.ExternalRef
import Reflex.Flunky
import Reflex.Fork
import Sepulcas.Native
import System.Timeout (timeout)

import qualified Control.Exception.Safe as Ex
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket.ByteString.Lazy as NSBL

import  Network.Socket.Manager.TCP.Client (PeekerIO, CloseReason (..), SocketStatus (..))
import  Network.Socket.Manager.Peeker 

-- | Possible exceptions when read from socket
type InboundException = Ex.SomeException
-- | Possible exceptions when establishing connection to remote host
type ConnectException = Ex.SomeException
-- | Possible exceptions when connection is closed ungracefully
type CloseException = Ex.SomeException

-- | Address of peer, service name is either port or port name
data Peer = Peer !N.HostName !N.ServiceName deriving (Show, Eq, Generic)

-- | Configuration to socket widget. Sending always raw bytestrings and receiving
-- is controlled by callind side with `Peeker`.
data SocketConf t a = SocketConf {
-- | Connection address
  _socketConfPeer   :: !Peer
-- | Event that socket will listen to payloads to send
, _socketConfSend   :: !(Event t ByteString)
-- | Function that peeks bytes from socket and parses them into user side messages
, _socketConfPeeker :: !(PeekerIO a)
-- | Event that externaly closes the connection
, _socketConfClose  :: !(Event t ())
-- | Configuration of SOCKS proxy. If the value changes, connection is reopened.
, _socketConfProxy  :: !(Dynamic t (Maybe SocksConf))
} deriving (Generic)

-- | Check whether the closing of socket is not recoverable
isCloseFinal :: CloseReason -> Bool
isCloseFinal r = case r of
  CloseGracefull -> True
  _ -> False

-- | Widget that is created with `socket` and allows to get results from
-- the socket.
data Socket t a = Socket {
-- | Inbound message
  _socketInbound :: !(Event t a)
-- | Fired when the socket closed
, _socketClosed  :: !(Event t CloseReason)
-- | Current status of socket
, _socketStatus  :: !(Dynamic t SocketStatus)
-- | Fires when failed to read bytes from socket
, _socketRecvEr  :: !(Event t InboundException)
-- | Amount of connection tries
, _socketTries   :: !(Dynamic t Int)
} deriving (Generic)

-- | Get event that fires each time the socket is connected to host
socketConnected :: Reflex t => Socket t a -> Event t ()
socketConnected Socket{..} = fforMaybe (updated _socketStatus) $ \case
  SocketOperational -> Just ()
  _ -> Nothing

-- | Mock socket that does nothing
noSocket :: Reflex t => Socket t a
noSocket = Socket {
    _socketInbound = never
  , _socketClosed = never
  , _socketStatus = pure SocketInitial
  , _socketRecvEr = never
  , _socketTries = pure 0
  }

-- | Switch over chain of sockets to represent only current one
switchSocket :: Reflex t => Dynamic t (Socket t a) -> Socket t a
switchSocket dsock = Socket {
    _socketInbound = switch . current $ _socketInbound <$> dsock
  , _socketClosed = switch . current $ _socketClosed <$> dsock
  , _socketStatus = join $ _socketStatus <$> dsock
  , _socketRecvEr = switch . current $ _socketRecvEr <$> dsock
  , _socketTries = join $ _socketTries <$> dsock
  }

-- | Widget that starts TCP socket internally and reconnects if needed.
socket :: (TriggerEvent t m, Adjustable t m, PerformEvent t m, MonadHold t m, PostBuild t m, MonadIO (Performable m), MonadIO m, MonadUnliftIO (Performable m), PlatformNatives)
  => SocketConf t a -> m (Socket t a)
socket SocketConf{..} = fmap switchSocket $ networkHoldDyn $ ffor _socketConfProxy $ \mproxy -> do
  buildE <- delay 0.01 =<< getPostBuild
  reconnTriesRef <- newExternalRef 0
  triesD <- externalRefDynamic reconnTriesRef
  (closeE, closeFire) <- newTriggerEvent
  (statusE, statusFire) <- newTriggerEvent
  (readErE, readErFire) <- newTriggerEvent
  (inE, inFire) <- newTriggerEvent
  statusD <- holdDyn SocketInitial statusE
  -- performEvent_ $ ffor closeE $ logWrite . showt
  -- performEvent_ $ ffor reconnectE $ logWrite . showt
  -- performEvent_ $ ffor (updated triesD) $ logWrite . showt
  let connectE = buildE
  let closeCb :: Maybe CloseException -> IO ()
      closeCb e = do
        -- logWrite $ showt e
        statusFire SocketClosed
        closeFire $ maybe CloseGracefull CloseError e
  intVar <- liftIO $ newTVarIO False
  sendChan <- liftIO newTChanIO
  performEvent_ $ ffor closeE $ const $ liftIO $ atomically $ writeTVar intVar True
  performEvent_ $ ffor _socketConfSend $ liftIO . atomically . writeTChan sendChan
  performEvent_ $ ffor _socketConfClose $ const $ liftIO $ closeCb Nothing
  performFork_  $ ffor connectE $ const $ liftIO $ do
    let sendThread sock = forever $ do
          msgs <- atomically $ readAllTVar sendChan
          -- logWrite $ "Sending message"
          sendLazy sock . BSL.fromChunks $ msgs
        conCb (sock, _) = do
          -- logWrite $ "Connected"
          statusFire SocketOperational
          let env = PeekerEnv intVar sock
          void $ forkOnOther $ sendThread sock
          fix $ \next -> do
            mma <- Ex.tryAny $ Ex.try $ runReaderT _socketConfPeeker env
            case mma of
              Left e -> readErFire e >> next
              Right (Left (e :: ReceiveException)) -> do
                readErFire $ Ex.SomeException e
                closeCb $ Just $ Ex.SomeException e
              Right (Right a) -> inFire a >> next
        Peer host port = _socketConfPeer
    -- logWrite $ "Connecting to " <> showt host <> ":" <> showt port
    statusFire SocketConnecting
    connect host port mproxy conCb `Ex.catchAny` (closeCb . Just)
  pure Socket {
      _socketInbound = inE
    , _socketClosed  = ffilter isCloseFinal closeE
    , _socketStatus  = statusD
    , _socketRecvEr  = readErE
    , _socketTries   = triesD
    }


readAllTVar :: TChan a -> STM [a]
readAllTVar chan = go []
  where
    go !acc = do
      a <- readTChan chan
      empty <- isEmptyTChan chan
      if empty then pure (a : acc) else go (a : acc)

-- * Note
-- We need to copy some functions from network-simple due problems with
-- crosscompilation to android. `network-bsd` cannot be linked against it. See
-- https://github.com/hexresearch/ergvein/issues/346

-- | Connect to a TCP server and use the connection.
--
-- The connection socket is shut down and closed when done or in case of
-- exceptions.
--
-- If you prefer to acquire and close the socket yourself, then use
-- 'connectSock' and 'closeSock'.
--
-- Note: The 'N.NoDelay' and 'N.KeepAlive' options are set on the socket.
connect
  :: (MonadIO m, Ex.MonadMask m)
  => N.HostName -- ^ Server hostname or IP address.
  -> N.ServiceName -- ^ Server service port name or number.
  -> Maybe SocksConf -- ^ Optional Socks proxy to use
  -> ((N.Socket, N.SockAddr) -> m r)
  -- ^ Computation taking the communication socket and the server address.
  -> m r
connect host port mproxy = Ex.bracket (connectSock host port mproxy) (closeSock . fst)

-- | Obtain a 'N.Socket' connected to the given host and TCP service port.
--
-- The obtained 'N.Socket' should be closed manually using 'closeSock' when
-- it's not needed anymore, otherwise you risk having the connection and socket
-- open for much longer than needed.
--
-- Prefer to use 'connect' if you will be using the socket within a limited
-- scope and would like it to be closed immediately after its usage or in case
-- of exceptions.
--
-- Note: The 'N.NoDelay' and 'N.KeepAlive' options are set on the socket.
connectSock
  :: MonadIO m
  => N.HostName -- ^ Server hostname or IP address.
  -> N.ServiceName -- ^ Server service port name or number.
  -> Maybe SocksConf -- ^ Optional Socks proxy to use
  -> m (N.Socket, N.SockAddr) -- ^ Connected socket and server address.
connectSock host port mproxy = liftIO $ do
    addrs <- N.getAddrInfo (Just hints) (Just host) (Just port)
    tryAddrs (happyEyeballSort addrs)
  where
    hints :: N.AddrInfo
    hints = N.defaultHints
      { N.addrFlags = [N.AI_ADDRCONFIG]
      , N.addrSocketType = N.Stream }
    tryAddrs :: [N.AddrInfo] -> IO (N.Socket, N.SockAddr)
    tryAddrs = \case
      [] -> fail "Network.Simple.TCP.connectSock: No addresses available"
      [x] -> useAddr x
      (x:xs) -> Ex.catch (useAddr x) (\(_ :: IOError) -> tryAddrs xs)
    useAddr :: N.AddrInfo -> IO (N.Socket, N.SockAddr)
    useAddr addr = do
       yx <- timeout 30000000 $ do -- 30 seconds
         let sockAddr = N.addrAddress addr
         sock <- case mproxy of
           Nothing -> Ex.bracketOnError (newSocket addr) closeSock $ \sock -> do
            N.setSocketOption sock N.NoDelay 1
            N.setSocketOption sock N.KeepAlive 1
            N.connect sock sockAddr
            pure sock
           Just proxy -> do
             (sock, _) <- socksConnect proxy $ fromSockAddr sockAddr
             pure sock
         pure (sock, sockAddr)
       case yx of
          Nothing -> fail "connectSock: Timeout on connect"
          Just x -> pure x

-- | Convert network address to format that socks library understands
fromSockAddr :: N.SockAddr -> SocksAddress
fromSockAddr = \case
  N.SockAddrInet port haddr -> SocksAddress (SocksAddrIPV4 haddr) port
  N.SockAddrInet6 port _ haddr _ -> SocksAddress (SocksAddrIPV6 haddr) port
  N.SockAddrUnix _ -> error "fromSockAddr: not supported unix socket"
  _ -> error "fromSockAddr: not supported socket"

newSocket :: N.AddrInfo -> IO N.Socket
newSocket addr = N.socket (N.addrFamily addr)
                          (N.addrSocketType addr)
                          (N.addrProtocol addr)

-- | Shuts down and closes the 'N.Socket', silently ignoring any synchronous
-- exception that might happen.
closeSock :: MonadIO m => N.Socket -> m ()
closeSock s = liftIO $ do
  Ex.catch (Ex.finally (N.shutdown s N.ShutdownBoth)
                       (N.close s))
           (\(_ :: Ex.SomeException) -> pure ())

-- | Writes a lazy 'BSL.ByteString' to the socket.
--
-- Note: This uses @writev(2)@ on POSIX.
sendLazy :: MonadIO m => N.Socket -> BSL.ByteString -> m ()
{-# INLINABLE sendLazy #-}
sendLazy sock = \lbs -> liftIO (NSBL.sendAll sock lbs)

-- | Read up to a limited number of bytes from a socket.
--
-- Returns `Nothing` if the remote end closed the connection or end-of-input was
-- reached. The number of returned bytes might be less than the specified limit,
-- but it will never 'BS.null'.
recv :: MonadIO m => N.Socket -> Int -> m (Maybe BS.ByteString)
recv sock nbytes = liftIO $ do
  bs <- liftIO (NSB.recv sock nbytes)
  if BS.null bs
     then pure Nothing
     else pure (Just bs)
{-# INLINABLE recv #-}

-- | Given a list of 'N.AddrInfo's, reorder it so that ipv6 and ipv4 addresses,
-- when available, are intercalated, with a ipv6 address first.
happyEyeballSort :: [N.AddrInfo] -> [N.AddrInfo]
happyEyeballSort l =
    concat (List.transpose ((\(a,b) -> [a,b]) (List.partition isIPv6addr l)))

isIPv6addr :: N.AddrInfo -> Bool
isIPv6addr x = N.addrFamily x == N.AF_INET6
