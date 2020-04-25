module Ergvein.Wallet.Node.Socket(
    InboundException
  , MonadPeeker(..)
  , Peer(..)
  , N.ServiceName
  , N.HostName
  , SocketConf(..)
  , SocketStatus(..)
  , CloseReason(..)
  , isCloseFinal
  , Socket(..)
  , noSocket
  , switchSocket
  , socket
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Default
import Data.Maybe
import Data.Time
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Native
import GHC.Generics
import Reflex

import qualified Control.Exception.Safe as Ex
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB
import qualified Network.Simple.TCP as N

-- | Possible exceptions when read from socket
type InboundException = Ex.SomeException
-- | Possible exceptions when establishing connection to remote host
type ConnectException = Ex.SomeException
-- | Possible exceptions when connection is closed ungracefully
type CloseException = Ex.SomeException

-- | Interface monad for `socket` widget that allows to peek exact amount of
-- bytes from socket to parse next portion of data.
class Monad m => MonadPeeker m where
  -- | Peek exact amount of bytes
  peek :: Int -> m ByteString

-- | Environment for `MonadPeeker` implementation
data PeekerEnv = PeekerEnv !(TVar Bool) !N.Socket

instance MonadIO m => MonadPeeker (ReaderT PeekerEnv m) where
  peek n = do
    PeekerEnv ivar sock <- ask
    liftIO $ receiveExactly ivar sock n
  {-# INLINE peek #-}

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
, _socketConfPeeker :: !(ReaderT PeekerEnv IO a)
-- | Event that closes the connection
, _socketConfClose  :: !(Event t ())
-- | Timeout after which we try to reconnect. `Nothing` means to not reconnect.
, _socketConfReopen :: !(Maybe NominalDiffTime)
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
  | CloseError !Bool !CloseException -- ^ Socket was closed by exception. Boolean marks is the connection restarting.
  deriving (Show, Generic)

-- | Check whether the closing of socket is not recoverable
isCloseFinal :: CloseReason -> Bool
isCloseFinal r = case r of
  CloseGracefull -> True
  CloseError b _ -> b

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
} deriving (Generic)

-- | Mock socket that does nothing
noSocket :: Reflex t => Socket t a
noSocket = Socket {
    _socketInbound = never
  , _socketClosed = never
  , _socketStatus = pure SocketInitial
  , _socketRecvEr = never
  }

-- | Switch over chain of sockets to represent only current one
switchSocket :: Reflex t => Dynamic t (Socket t a) -> Socket t a
switchSocket dsock = Socket {
    _socketInbound = switch . current $ _socketInbound <$> dsock
  , _socketClosed = switch . current $ _socketClosed <$> dsock
  , _socketStatus = join $ _socketStatus <$> dsock
  , _socketRecvEr = switch . current $ _socketRecvEr <$> dsock
  }

-- | Widget that starts TCP socket internally and reconnects if needed.
socket :: (TriggerEvent t m, PerformEvent t m, MonadHold t m, PostBuild t m, MonadIO (Performable m), MonadIO m, MonadUnliftIO (Performable m), PlatformNatives)
  => SocketConf t a -> m (Socket t a)
socket SocketConf{..} = do
  buildE <- delay 0.01 =<< getPostBuild
  (closeE, closeFire) <- newTriggerEvent
  (statusE, statusFire) <- newTriggerEvent
  (readErE, readErFire) <- newTriggerEvent
  (inE, inFire) <- newTriggerEvent
  statusD <- holdDyn SocketInitial statusE
  let doReconnecting = isJust _socketConfReopen
  reconnectE <- case _socketConfReopen of
    Just dt -> delay dt $ fforMaybe closeE $ \cr -> if isCloseFinal cr then Nothing else Just ()
    _ -> pure never
  let connectE = leftmost [reconnectE, buildE]
  intVar <- liftIO $ newTVarIO False
  sendChan <- liftIO newTChanIO
  performEvent_ $ ffor closeE $ const $ liftIO $ atomically $ writeTVar intVar True
  performEvent_ $ ffor reconnectE $ const $ liftIO $ atomically $ writeTVar intVar True
  performEvent_ $ ffor _socketConfSend $ liftIO . atomically . writeTChan sendChan
  performFork_ $ ffor connectE $ const $ do
    let closeCb e = closeFire $ CloseError doReconnecting e
        sendThread sock = forever $ do
          msgs <- atomically $ readAllTVar sendChan
          N.sendLazy sock . BSL.fromChunks $ msgs
        conCb (sock, _) = do
          let env = PeekerEnv intVar sock
          void $ forkIO $ sendThread sock
          forever $ do
            ma <- Ex.tryAny $ runReaderT _socketConfPeeker env
            either readErFire inFire ma
        Peer host port = _socketConfPeer
    liftIO $ N.connect host port conCb `Ex.catchAny` closeCb
  pure Socket {
      _socketInbound = inE
    , _socketClosed  = closeE
    , _socketStatus  = statusD
    , _socketRecvEr  = readErE
    }

data ReceiveException = ReceiveEndOfInput | ReceiveInterrupted
  deriving (Eq, Show, Generic)

instance Ex.Exception ReceiveException

-- | Helper to read excact amount of bytes from socket with possible interruption.
receiveExactly :: TVar Bool -> N.Socket -> Int -> IO ByteString
receiveExactly intVar sock n = go n mempty
  where
    go i acc = do
      needExit <- liftIO . atomically $ readTVar intVar
      when needExit $ Ex.throw ReceiveInterrupted
      mbs <- N.recv sock i
      case mbs of
        Nothing -> Ex.throw ReceiveEndOfInput
        Just bs -> let
          l = BS.length bs
          acc' = acc <> BB.byteString bs
          in if l < i then go (i - l) acc' else pure . BSL.toStrict . BB.toLazyByteString $ acc

readAllTVar :: TChan a -> STM [a]
readAllTVar chan = go []
  where
    go !acc = do
      mres <- tryReadTChan chan
      maybe (pure acc) (go . (: acc)) mres
