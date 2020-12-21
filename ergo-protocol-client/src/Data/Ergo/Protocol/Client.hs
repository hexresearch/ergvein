module Data.Ergo.Protocol.Client(
  -- * Exceptions
    C.InboundException
  , C.ConnectException
  , C.CloseException
  -- * Configuration
  , C.Peer(..)
  , C.SocketInEvent(..)
  , C.SocketConf(..)
  -- * Socket
  , C.SocketOutEvent(..)
  , ergoSocket
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Ergo.Protocol
import Data.Ergo.Protocol.Decoder
import Data.IORef
import GHC.Generics

import Debug.Trace

import qualified Network.Socket.Manager.TCP.Client as C

peekMessage :: Network -> IORef Bool -> C.PeekerIO Message
peekMessage net initRef = do
  isinit <- liftIO $ readIORef initRef
  if isinit then do
    h <- peekHandshake (fmap traceShowId . C.peek . traceShowId)
    liftIO $ writeIORef initRef False
    pure $ MsgHandshake h
  else do
    n <- either fail pure =<< fmap (parseMsgLength net) (C.peek 9)
    either fail pure =<< fmap (decodeMessage net) (C.peek n)

-- | Start connection to ergo node in separate thread.
--
-- Automatically reopens on non user triggered close events (if config specifty this)
-- and when SOCKS config is changed.
--
-- The socket doesn't perform handshaking with node.
ergoSocket :: MonadIO m
  => Network
  -> TChan (C.SocketInEvent Message)
  -> C.SocketConf
  -> m (TChan (C.SocketOutEvent Message))
ergoSocket net inChan conf = do
  initRef <- liftIO $ newIORef True
  C.socket (encodeMessage net) (peekMessage net initRef) inChan conf
