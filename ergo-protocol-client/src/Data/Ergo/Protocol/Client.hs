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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Ergo.Protocol
import Data.Ergo.Protocol.Decoder
import Data.IORef
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics

import Debug.Trace
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16

import qualified Network.Socket.Manager.TCP.Client as C

makeHandshake :: Int -> UTCTime -> Handshake
makeHandshake blocks t = Handshake {
    time = round $ utcTimeToPOSIXSeconds t
  , agentName = "ergoref"
  , version = ProtoVer 4 0 2
  , peerName = "ergo-mainnet-4.0.0"
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

peekMessage :: Network -> IORef Bool -> C.PeekerIO ErgoMessage
peekMessage net initRef = do
  isinit <- liftIO $ readIORef initRef
  if isinit then do
    bs <- C.peekAll
    -- liftIO $ putStrLn $ "HANDSHAKE: " <> (show . B16.encode) bs
    h <- either fail pure $ decodeHandshake bs
    liftIO $ writeIORef initRef False
    pure $ MsgHandshake h
  else do
    bs <- C.peekAll
    (i, n) <- either fail pure $ parseMsgLength net $ BS.take 9 bs
    either fail (pure . MsgOther) $ parseMsgBody i n $ BS.drop 9 bs

-- | Start connection to ergo node in separate thread.
--
-- Automatically reopens on non user triggered close events (if config specifty this)
-- and when SOCKS config is changed.
--
-- The socket doesn't perform handshaking with node.
ergoSocket :: MonadIO m
  => Network
  -> TChan (C.SocketInEvent ErgoMessage)
  -> C.SocketConf
  -> m (TChan (C.SocketOutEvent ErgoMessage))
ergoSocket net inChan conf = do
  initRef <- liftIO $ newIORef True
  let encoder net msg = encodeErgoMessage net msg
  out <- C.socket (encoder net) (peekMessage net initRef) inChan conf
  liftIO $ do
    outInt <- atomically $ dupTChan out
    void $ forkIO $ forever $ do
      e <- atomically $ readTChan outInt
      case e of
        C.SockOutTries _ -> writeIORef initRef True
        _ -> pure ()
  pure out
