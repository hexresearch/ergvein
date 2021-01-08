{-# LANGUAGE OverloadedLists #-}
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
  , makeHandshake
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Ergo.Protocol
import Data.Ergo.Protocol.Decoder
import Data.IORef
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics

import Debug.Trace
import qualified Data.ByteString.Base16 as B16

import qualified Network.Socket.Manager.TCP.Client as C

makeHandshake :: Int -> UTCTime -> Handshake
makeHandshake blocks t = Handshake {
    time = round $ utcTimeToPOSIXSeconds t
  , agentName = "ergoref"
  , version = ProtoVer 3 3 6
  , peerName = "ergo-mainnet-3.3.6"
  , publicAddr = Nothing
  , peerFeatures = [
      FeatureOperationMode OperationModeFeature {
        stateType     = StateUtxo
      , verifying     = False
      , nipopowSuffix = Nothing
      , blocksStored  = fromIntegral blocks
      }
    ]
  }

peekMessage :: Network -> IORef Bool -> C.PeekerIO Message
peekMessage net initRef = do
  isinit <- liftIO $ readIORef initRef
  if isinit then do
    bs <- C.peekAll
    traceShowM bs
    traceShowM $ B16.encode bs
    h <- either fail pure $ decodeHandshake bs
    traceShowM h
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
