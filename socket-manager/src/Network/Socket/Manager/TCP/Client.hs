module Network.Socket.Manager.TCP.Client(
  -- * Exceptions
    InboundException
  , ConnectException
  , CloseException
  -- * Configuration
  , Peer(..)
  , SocksConf(..)
  , SocketInEvent(..)
  , SocketConf(..)
  -- * Widget
  , SocketStatus(..)
  , SocketOutEvent(..)
  , CloseReason(..)
  , socket
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Time
import GHC.Generics
import Network.Socket.Manager.Peeker
import Network.Socks5 (SocksConf(..), socksConnect, SocksAddress(..), SocksHostAddress(..))
import System.Timeout (timeout)

import qualified Control.Concurrent.Thread.Delay as UD
import qualified Control.Exception.Safe as Ex
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Network.Socket as N
import qualified Network.Socket.ByteString.Lazy as NSBL

-- | Possible exceptions when read from socket
type InboundException = Ex.SomeException
-- | Possible exceptions when establishing connection to remote host
type ConnectException = Ex.SomeException
-- | Possible exceptions when connection is closed ungracefully
type CloseException = Ex.SomeException

-- | Address of peer, service name is either port or port name
data Peer = Peer !N.HostName !N.ServiceName deriving (Show, Eq, Generic)

-- | Control events of the socket
data SocketInEvent a =
  -- | Payload to send into socket
    SockInSendEvent !a
  -- | Event that externaly closes the connection
  | SockInCloseEvent
  -- | Event that changes socks proxy configuration. Connection reopens.
  | SockInSocksConf !(Maybe SocksConf)
  deriving (Generic, Functor)

-- | Configuration to socket widget. Sending always raw bytestrings and receiving
-- is controlled by callind side with `Peeker`.
data SocketConf = SocketConf {
-- | Connection address
  _socketConfPeer   :: !Peer
-- | Initial SOCKS proxy configuration
, _socketConfSocks  :: !(Maybe SocksConf)
-- | Timeout after which we try to reconnect. `Nothing` means to not reconnect.
-- Second number means amount of tries of reconnection after which close event is fired.
, _socketConfReopen :: !(Maybe (NominalDiffTime, Int))
} deriving (Generic)

-- | Different socket states
data SocketStatus =
    SocketInitial -- ^ Initial state of socket after creation
  | SocketConnecting -- ^ Connection process in progress including reconnecting.
  | SocketOperational -- ^ Work state of socket
  | SocketClosed -- ^ Final state of socket
  deriving (Eq, Ord, Show, Read, Generic)

-- | Information why socket was closed
data CloseReason =
    CloseGracefull -- ^ Socket was closed gracefully and not going to be reopened
  | CloseError !CloseException -- ^ Socket was closed by exception. Boolean marks is the connection restarting.
  deriving (Show, Generic)

-- | Messages that user get from socket
data SocketOutEvent a =
    SockOutInbound !a
  | SockOutClosed  !CloseReason
  | SockOutStatus  !SocketStatus
  | SockOutRecvEr  !InboundException
  | SockOutTries   !Int
  deriving (Show, Generic, Functor)

data Reconnect = DoReconnect !(Maybe Ex.SomeException) | GraceStop

-- | Monad for parsing incoming messages
type PeekerIO a = ReaderT PeekerEnv IO a

-- | Start socket in separate thread and send events about it state change.
--
-- Automatically reopens on non user triggered close events (if config specifty this)
-- and when SOCKS config is changed.
socket :: (MonadIO m)
  => PeekerIO a -- ^ Deserialization of outcoming messages
  -> TChan (SocketInEvent ByteString) -- ^ Incoming messages
  -> SocketConf
  -> m (TChan (SocketOutEvent a))
socket peeker inputChan SocketConf{..} = liftIO $ do
  eventsChan <- newTChanIO
  sendChan <- newTChanIO
  intVar <- newTVarIO False
  socksVar <- newTVarIO _socketConfSocks
  reconChan <- newTChanIO
  connThreadVar <- newTVarIO Nothing
  triesVar <- newTVarIO 0

  let outEvFire = atomically . writeTChan eventsChan
  let statusFire = outEvFire . SockOutStatus
  let readErFire = outEvFire . SockOutRecvEr
  let inFire = outEvFire . SockOutInbound
  let closeCb = atomically . writeTChan reconChan . DoReconnect

  -- Thread that process input events
  inputThread <- forkIO $ forever $ do
    e <- atomically $ readTChan inputChan
    case e of
      SockInSendEvent bs -> atomically $ writeTChan sendChan bs
      SockInCloseEvent -> atomically $ writeTChan reconChan GraceStop
      SockInSocksConf sconf -> atomically $ do
          writeTVar socksVar sconf
          writeTChan reconChan $ DoReconnect Nothing

  -- Socket connection/reading and sending threads
  let connectThread = do
        let sendThread sock = forever $ do
              msgs <- atomically $ readAllTChan sendChan
              sendLazy sock . BSL.fromChunks $ msgs
            conCb (sock, _) = do
              statusFire SocketOperational
              let env = PeekerEnv intVar sock
              void $ forkIO $ sendThread sock
              fix $ \next -> do
                mma <- Ex.tryAny $ Ex.try $ runReaderT peeker env
                case mma of
                  Left e -> readErFire e >> next
                  Right (Left (e :: ReceiveException)) -> do
                    readErFire $ Ex.SomeException e
                    closeCb $ Just $ Ex.SomeException e
                  Right (Right a) -> inFire a >> next
            Peer host port = _socketConfPeer
        statusFire SocketConnecting
        mproxy <- atomically . readTVar $ socksVar
        connect host port mproxy conCb `Ex.catchAny` (closeCb . Just)

  -- Reconnection thread
  _ <- forkIO $ fix $ \next -> do
    e <- atomically $ readTChan reconChan
    case e of
      DoReconnect Nothing -> do
        cid <- atomically $ readTVar connThreadVar
        traverse_ killThread cid
        atomically $ writeTVar triesVar 0
        connTid <- forkIO connectThread
        atomically $ writeTVar connThreadVar $ Just connTid
        next
      DoReconnect (Just ex) -> do
        i <- atomically $ readTVar triesVar
        case _socketConfReopen of
          Nothing -> do
            killThread inputThread
            atomically $ writeTChan eventsChan $ SockOutClosed $ CloseError ex
          Just (_, n) | i >= n -> do
            killThread inputThread
            atomically $ writeTChan eventsChan $ SockOutClosed $ CloseError ex
          Just (dt, _) -> do
            atomically $ writeTChan eventsChan $ SockOutTries i
            _ <- forkIO $ do
              UD.delay $ ceiling $ (realToFrac dt :: Double) * 1000000
              connTid <- forkIO connectThread
              atomically $ modifyTVar' triesVar (+1)
              atomically $ writeTVar connThreadVar $ Just connTid
            next
      GraceStop -> do
        cid <- atomically $ readTVar connThreadVar
        traverse_ killThread cid
        killThread inputThread
        atomically $ writeTChan eventsChan $ SockOutClosed CloseGracefull

  -- Initial spawn of connection
  connTid <- forkIO connectThread
  atomically $ writeTVar connThreadVar $ Just connTid

  pure eventsChan

readAllTChan :: TChan a -> STM [a]
readAllTChan chan = go []
  where
    go !acc = do
      a <- readTChan chan
      empty <- isEmptyTChan chan
      if empty then pure (a : acc) else go (a : acc)

-- | Writes a lazy 'BSL.ByteString' to the socket.
--
-- Note: This uses @writev(2)@ on POSIX.
sendLazy :: MonadIO m => N.Socket -> BSL.ByteString -> m ()
{-# INLINABLE sendLazy #-}
sendLazy sock = \lbs -> liftIO (NSBL.sendAll sock lbs)

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

-- | Given a list of 'N.AddrInfo's, reorder it so that ipv6 and ipv4 addresses,
-- when available, are intercalated, with a ipv6 address first.
happyEyeballSort :: [N.AddrInfo] -> [N.AddrInfo]
happyEyeballSort l =
    concat (List.transpose ((\(a,b) -> [a,b]) (List.partition isIPv6addr l)))

isIPv6addr :: N.AddrInfo -> Bool
isIPv6addr x = N.addrFamily x == N.AF_INET6
