{-
  Implementation of Ergo connector
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedLists #-}
module Ergvein.Core.Node.Ergo
  (
    ErgoType(..)
  , NodeErgo
  , initErgoNode
  ) where

import Control.Monad.IO.Class
import Data.Ergo.Protocol.Client
import Data.Ergo.Protocol.Decoder
import Data.Ergo.Protocol.Encoder
import Data.Ergo.Protocol.Types
import Data.Serialize (decode, runGet, runPut)
import Data.Text
import Data.Function
import Data.Time.Clock
import Network.Socket (SockAddr)
import Network.Socket (getNameInfo, NameInfoFlag(..), SockAddr(..))
import Reflex
import Reflex.ExternalRef
import Reflex.Fork
import Reflex.Network
import Sepulcas.Log
import Sepulcas.Native
import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import Ergvein.Core.Node.Socket hiding (Peer, SocketConf)
import Ergvein.Core.Node.Types
import Ergvein.Core.Platform
import Ergvein.Core.Settings
import Ergvein.Text
import Ergvein.Types.Currency
import Control.Monad
import Data.Ergo.Modifier

instance CurrencyRep ErgoType where
  curRep _ = ERGO

instance HasNode ErgoType where
  type NodeReq ErgoType = ErgoMessage
  type NodeResp ErgoType = ErgoMessage
  type NodeSpecific ErgoType = ()

initErgoNode :: (MonadSettings t m) => SockAddr -> Event t NodeMessage -> m (NodeErgo t)
initErgoNode url msgE = mdo
  buildE <- getPostBuild
  proxyE <- fmap SockInSocksConf . updated <$> getSocksConf
  let restartE = fforMaybe msgE $ \case
        NodeMsgRestart -> Just ()
        _ -> Nothing
      closeE = fforMaybe msgE $ \case
        NodeMsgClose -> Just SockInCloseEvent
        _ -> Nothing
      socketInUserE = fforMaybe msgE $ \case
        NodeMsgReq (NodeReqErgo req) -> Just $ SockInSendEvent req
        _ -> Nothing
      socketInSystemE = leftmost [proxyE, closeE]
      startE = leftmost [buildE, restartE]
      net = ergoNetwork
  peerE <- performFork $ ffor startE $ const $ do
    (Just sname, Just sport) <- liftIO $ getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True url
    pure $  Peer sname sport

  -- Create channel before socket as there can be inbound messages between moment of widget creation and
  -- switching to new socket. Otherwise we can get in race where messages are not detected at start time
  inChan <- liftIO newTChanIO
  -- Messages that reconnects socket or closes it we send without waiting for handshake
  performEvent_ $ ffor socketInSystemE $ liftIO . atomically . writeTChan inChan
  -- Messages that can sand user of the widget is sent only when socket finally has been handshaked
  performFork_ $ ffor socketInUserE $ \ m -> do
    -- TODO: Note that order of messages can be broken here. That shouldn't be an issue as these are initial
    -- messages and protocol itself asynchronous.
    isShaked <- sample . current $ shakeD
    unless isShaked $ fix $ \next -> do
      liftIO $ threadDelay 1000
      shaked <- sample . current $ shakeD
      unless shaked next

    liftIO $ print "TEEEEEEEEEEEEEEEEE"
    liftIO . atomically . writeTChan inChan $ m
    liftIO $ print $ "REEEEEEEEEEEEEE"

  socket <- fmap switchSocket $ networkHold (pure noSocket) $ ffor peerE $ \peer -> do

    liftIO $ print "performFork"
    currentTime <- liftIO getCurrentTime
    liftIO . atomically . writeTChan inChan $ SockInSendEvent $ MsgHandshake $ makeHandshake 0 currentTime

    outChan <- ergoSocket net inChan $ SocketConf {
            _socketConfPeer = peer
          , _socketConfSocks = Nothing
          , _socketConfReopen = Just (3.0, 5)
      }
    (closedE,  closedFire)   <- newTriggerEvent
    (messageE, messageFire)  <- newTriggerEvent
    (statusE,  statusFire)   <- newTriggerEvent
    (errorE,   errorFire)    <- newTriggerEvent
    (triesE,   triesFire)    <- newTriggerEvent

    inputThread <- liftIO $ forkIO $ forever $ do
      msg <- atomically $ readTChan outChan
      print $ show msg
      case msg of
        SockOutInbound message     -> messageFire message
        SockOutClosed  closeReason -> closedFire  closeReason
        SockOutStatus  status      -> statusFire  status
        SockOutRecvEr  err         -> errorFire   err
        SockOutTries   tries       -> triesFire   tries

    performEvent $ ffor closedE $ const $ liftIO $ killThread inputThread

    statusD <-  holdDyn SocketInitial statusE
    triesD <-  holdDyn 0 triesE

    pure $ Socket {  _socketInbound = messageE
                  , _socketClosed = closedE
                  , _socketStatus = statusD
                  , _socketRecvEr = errorE
                  , _socketTries = triesD
                  }

  statRef <- newExternalRef Nothing
  let respE   = _socketInbound socket
      verAckE = fforMaybe respE $ \case
        MsgHandshake _ -> Just True
        _ -> Nothing
      closedE = void $ _socketClosed socket

  shakeD <- holdDyn False $ leftmost [verAckE, False <$ closeE]

  let openE = void $ ffilter id $ updated shakeD

  pure $ NodeConnection {
      nodeconCurrency   = ERGO
    , nodeconUrl        = url
    , nodeconStatus     = statRef
    , nodeconOpensE     = openE
    , nodeconCloseE     = closedE
    , nodeconRespE      = respE
    , nodeconExtra      = ()
    , nodeconIsUp       = shakeD
    , nodeconDoLog      = False
    , nodeconHeight     = pure Nothing
    }
