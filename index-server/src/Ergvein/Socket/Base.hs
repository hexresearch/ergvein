module Ergvein.Socket.Base
  (
    connect
  , MonadPeeker(..)
  , PeekerEnv(..)
  , ReceiveException(..)
  -- TODO: These are not used. Investigate
  , InboundException
  , ConnectException
  , isCloseFinal
  , readAllTVar
  , sendLazy
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Unlift
import Data.ByteString (ByteString)
import GHC.Generics
import qualified Control.Exception.Safe as Ex
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NSB
import qualified Data.List as List
import qualified Network.Socket.ByteString.Lazy as NSBL
import System.Timeout (timeout)
import Control.Monad.Reader


-- | Possible exceptions when read from socket
type InboundException = Ex.SomeException
-- | Possible exceptions when establishing connection to remote host
type ConnectException = Ex.SomeException
-- | Possible exceptions when connection is closed ungracefully
type CloseException = Ex.SomeException

-- | Interface monad for `socket` widget that allows to peek exact amount of
-- bytes from socket to parse next portion of data.
class Monad m => MonadPeeker m where
  -- | Peek exact amount of bytes. Can throw `ReceiveException` when connection
  -- is closed from other side or broken.
  peek :: Int -> m ByteString

-- | Environment for `MonadPeeker` implementation
data PeekerEnv = PeekerEnv !(TVar Bool) !N.Socket

-- | Environment for `MonadPeeker` implementation
instance MonadIO m => MonadPeeker (ReaderT PeekerEnv m) where
  peek n = do
    PeekerEnv ivar sock <- ask
    liftIO $ receiveExactly ivar sock n
  {-# INLINE peek #-}

-- | Address of peer, service name is either port or port name
data Peer = Peer !N.HostName !N.ServiceName deriving (Show, Eq, Generic)

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
  | CloseError !Bool !CloseException -- ^ Socket was closed by exception. Boolean marks is the connection restarting.
  deriving (Show, Generic)

-- | Check whether the closing of socket is not recoverable
isCloseFinal :: CloseReason -> Bool
isCloseFinal r = case r of
  CloseGracefull -> True
  CloseError b _ -> not b

-- | Exception occured when receiving bytes from socket fails.
data ReceiveException =
    ReceiveEndOfInput -- ^ Connection was closed from other side
  | ReceiveInterrupted -- ^ Receiving was interrupted by our side
  deriving (Eq, Show, Generic)

instance Ex.Exception ReceiveException

-- | Helper to read excact amount of bytes from socket with possible interruption.
receiveExactly :: TVar Bool -> N.Socket -> Int -> IO ByteString
receiveExactly intVar sock n = go n mempty
  where
    go i acc = do
      needExit <- liftIO . atomically $ readTVar intVar
      when needExit $ Ex.throw ReceiveInterrupted
      mbs <- recv sock i
      case mbs of
        Nothing -> Ex.throw ReceiveEndOfInput
        Just bs -> let
          l = BS.length bs
          acc' = acc <> BB.byteString bs
          in if l < i then go (i - l) acc' else pure . BSL.toStrict . BB.toLazyByteString $ acc'

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
  -> ((N.Socket, N.SockAddr) -> m r)
  -- ^ Computation taking the communication socket and the server address.
  -> m r
connect host port = Ex.bracket (connectSock host port) (closeSock . fst)

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
  -> m (N.Socket, N.SockAddr) -- ^ Connected socket and server address.
connectSock host port = liftIO $ do
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
       yx <- timeout 3000000 $ do -- 3 seconds
          Ex.bracketOnError (newSocket addr) closeSock $ \sock -> do
             let sockAddr = N.addrAddress addr
             N.setSocketOption sock N.NoDelay 1
             N.setSocketOption sock N.KeepAlive 1
             N.connect sock sockAddr
             pure (sock, sockAddr)
       case yx of
          Nothing -> fail "Network.Simple.TCP.connectSock: Timeout on connect"
          Just x -> pure x

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
