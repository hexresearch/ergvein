{-
  Implementation of Ergo connector
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Core.Node.Ergo
  (
    ErgoType(..)
  , NodeErgo
  , initErgoNode
  ) where

import Control.Monad.IO.Class
import Data.Ergo.Protocol.Client hiding (SocketConf, Peer, PeekerEnv)
import qualified Data.Ergo.Protocol.Client as EEE
import Data.Ergo.Protocol.Decoder
import Data.Ergo.Protocol.Encoder
import Data.Ergo.Protocol.Types
import Data.Serialize (decode, runGet, runPut)
import Data.Text
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

import Ergvein.Core.Node.Socket
import Ergvein.Core.Node.Types
import Ergvein.Core.Platform
import Ergvein.Core.Settings
import Ergvein.Text
import Ergvein.Types.Currency
import Control.Monad
import  Control.Monad.Fix

instance CurrencyRep ErgoType where
  curRep _ = ERGO

instance HasNode ErgoType where
  type NodeReq ErgoType = ErgoMessage
  type NodeResp ErgoType = ErgoMessage
  type NodeSpecific ErgoType = ()

initErgoNode :: (MonadSettings t m) => SockAddr -> Event t NodeMessage -> m (NodeErgo t)
initErgoNode url msgE = do
  buildE <- getPostBuild
  let restartE = fforMaybe msgE $ \case
        NodeMsgRestart -> Just ()
        _ -> Nothing
      closeE = fforMaybe msgE $ \case
        NodeMsgClose -> Just ()
        _ -> Nothing
      reqE = fforMaybe msgE $ \case
        NodeMsgReq (NodeReqErgo req) -> Just req
        _ -> Nothing
  
  let startE = leftmost [buildE, restartE]
  let net = ergoNetwork
      nodeLog :: MonadIO m => Text -> m ()
      nodeLog =  logWrite . (nodeString ERGO url <>)
  peerE <- performFork $ ffor never $ const $ do
    (Just sname, Just sport) <- liftIO $ getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True url
    pure $  EEE.Peer sname sport
  proxyD <- getSocksConf
  
  rec
    isInit <- liftIO $ newIORef True
    
    s <- fmap switchSocket $ networkHold (pure noSocket) $ ffor peerE $ \peer -> do
        inChan <- liftIO newTChanIO
        outChan <- ergoSocket net inChan $ EEE.SocketConf {
              _socketConfPeer = peer
              , _socketConfSocks = Nothing
              , _socketConfReopen = Just (3.0, 5)
          }
        (e, cb) <- newTriggerEvent
        inputThread <- liftIO $  forkIO $ forever $ cb =<< (atomically $ readTChan outChan)
        
        let cE = fforMaybe e $ \case
                  SockOutClosed r -> Just r
                  _ -> Nothing

        let iE = fforMaybe e $ \case
                  SockOutInbound r -> Just r
                  _ -> Nothing
        
        let sE = fforMaybe e $ \case
                  SockOutStatus r -> Just r
                  _ -> Nothing

        let errE = fforMaybe e $ \case
                  SockOutRecvEr r -> Just r
                  _ -> Nothing
        let triesE = fforMaybe e $ \case
                  SockOutTries r -> Just r
                  _ -> Nothing

        performEvent_ $ ffor cE $ const $ liftIO $ killThread inputThread

        statD <-  holdDyn SocketInitial sE

        triesD <-  holdDyn 0 triesE

        pure $ Socket {
                 _socketInbound = iE
               , _socketClosed = cE
               , _socketStatus = statD
               , _socketRecvEr = errE
               , _socketTries = triesD
               }
    let respE = _socketInbound s
    handshakeE <- performEvent $ ffor (socketConnected s) $ const $ do
        ct <- liftIO getCurrentTime
        pure $ MsgHandshake $ makeHandshake 0 ct

  performEvent_ $ ffor (_socketRecvEr s) $ nodeLog . showt
  proxyD <- getSocksConf

  statRef <- newExternalRef Nothing

  let verAckE = fforMaybe respE $ \case
        MsgHandshake _ -> Just True
        _ -> Nothing

  shakeD <- holdDyn False $ leftmost [verAckE, False <$ closeE]
  let openE = fmapMaybe (\o -> if o then Just () else Nothing) $ updated shakeD
      closedE = () <$ _socketClosed s

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
