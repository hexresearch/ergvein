{-
  Implementation of Ergo connector
-}
module Ergvein.Core.Node.Ergo
  (
    ErgoType(..)
  , NodeErgo
  , initErgoNode
  , newSocket
  , f
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Ergo.Protocol.Client
import Data.Ergo.Protocol.Types
import Data.Function
import Data.Text
import Data.Time.Clock
import Network.Socket (getNameInfo, NameInfoFlag(..), SockAddr(..))
import Reflex
import Reflex.ExternalRef
import Reflex.Flunky
import Reflex.Fork
import Reflex.Network
import Control.Monad.Fail
import Control.Monad.Fix

import Ergvein.Core.Node.Socket hiding (Peer, SocketConf)
import Ergvein.Core.Node.Types
import Ergvein.Core.Platform
import Ergvein.Core.Settings
import Ergvein.Types.Currency

import qualified Network.Socks5 as S5

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
  performFork_ $ ffor socketInUserE $ \ msg -> do
    -- TODO: Note that order of messages can be broken here. That shouldn't be an issue as these are initial
    -- messages and protocol itself asynchronous.
    isShaked <- sample . current $ shakeD
    unless isShaked $ fix $ \next -> do
      liftIO $ threadDelay 1000
      shaked <- sampleDyn shakeD
      unless shaked next
    liftIO $ atomically $ writeTChan inChan msg 
  socket <- switchSocket <$> (networkHold (pure noSocket) $ ffor peerE $ outChanToSocket <=< newSocket net inChan)

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
    , nodeconOpensE     = openE
    , nodeconCloseE     = closedE
    , nodeconRespE      = respE
    , nodeconExtra      = ()
    , nodeconIsUp       = shakeD
    , nodeconDoLog      = False
    , nodeconHeight     = pure Nothing
    }
f :: (Adjustable t m,
      MonadFail (Performable m),
      MonadFix m,
      MonadHold t m,
      MonadIO m,
      MonadUnliftIO (Performable m),
      MonadSample t (Performable m),
      PerformEvent t m,
      PostBuild t m,
      TriggerEvent t m
      ) =>
   SockAddr -> Event t NodeMessage -> Event t (Maybe S5.SocksConf) ->  m (NodeErgo t)
f  url msgE dyn = mdo
  buildE <- getPostBuild
  let proxyE = SockInSocksConf <$> dyn
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
  performFork_ $ ffor socketInUserE $ \ msg -> do
    -- TODO: Note that order of messages can be broken here. That shouldn't be an issue as these are initial
    -- messages and protocol itself asynchronous.
    isShaked <- sample . current $ shakeD
    unless isShaked $ fix $ \next -> do
      liftIO $ threadDelay 1000
      shaked <- sampleDyn shakeD
      unless shaked next
    liftIO $ atomically $ writeTChan inChan msg 
  socket <- switchSocket <$> (networkHold (pure noSocket) $ ffor peerE $ outChanToSocket <=< newSocket net inChan)

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
    , nodeconOpensE     = openE
    , nodeconCloseE     = closedE
    , nodeconRespE      = respE
    , nodeconExtra      = ()
    , nodeconIsUp       = shakeD
    , nodeconDoLog      = False
    , nodeconHeight     = pure Nothing
    }

newSocket :: (MonadIO m) => Network -> TChan (SocketInEvent ErgoMessage) -> Peer -> m (TChan (SocketOutEvent ErgoMessage))
newSocket net inChan peer = do
    currentTime <- liftIO getCurrentTime
    liftIO . atomically . writeTChan inChan $ SockInSendEvent $ MsgHandshake $ makeHandshake 0 currentTime
    ergoSocket net inChan $ SocketConf {
            _socketConfPeer = peer
          , _socketConfSocks = Nothing
          , _socketConfReopen = Just (3.0, 5)
      }

outChanToSocket :: (TriggerEvent t m, PerformEvent t m, MonadHold t m, MonadIO m, MonadUnliftIO (Performable m)) => 
  TChan (SocketOutEvent a) -> m (Socket t a)
outChanToSocket outChan = do
  (closedE,  closedFire)   <- newTriggerEvent
  (messageE, messageFire)  <- newTriggerEvent
  (statusE,  statusFire)   <- newTriggerEvent
  (errorE,   errorFire)    <- newTriggerEvent
  (triesE,   triesFire)    <- newTriggerEvent
  
  inputThread <- liftIO $ forkIO $ forever $ do
    msg <- atomically $ readTChan outChan
    case msg of
      SockOutInbound message     -> messageFire message
      SockOutClosed  closeReason -> closedFire  closeReason
      SockOutStatus  status      -> statusFire  status
      SockOutRecvEr  err         -> errorFire   err
      SockOutTries   tries       -> triesFire   tries
  
  performEvent $ ffor closedE $ const $ liftIO $ killThread inputThread
  
  statusD <-  holdDyn SocketInitial statusE
  triesD <-  holdDyn 0 triesE
  
  pure $ Socket { _socketInbound = messageE
                , _socketClosed  = closedE
                , _socketStatus  = statusD
                , _socketRecvEr  = errorE
                , _socketTries   = triesD
                }