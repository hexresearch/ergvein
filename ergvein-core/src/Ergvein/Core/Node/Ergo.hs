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
import Data.Ergo.Protocol.Decoder
import Data.Ergo.Protocol.Encoder
import Data.Ergo.Protocol.Types
import Data.Serialize (decode, runGet, runPut)
import Data.Text
import Data.Time.Clock
import Ergvein.Core.Node.Socket
import Ergvein.Core.Node.Types
import Ergvein.Core.Platform
import Ergvein.Core.Settings
import Ergvein.Text
import Ergvein.Types.Currency
import Network.Socket (SockAddr)
import Network.Socket (getNameInfo, NameInfoFlag(..), SockAddr(..))
import Reflex
import Reflex.ExternalRef
import Reflex.Fork
import Reflex.Network
import Sepulcas.Log
import Sepulcas.Native

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
    pure $  Peer sname sport
  proxyD <- getSocksConf
  rec
    s <- fmap switchSocket $ networkHold (pure noSocket) $ ffor peerE $ \peer -> do
        socket SocketConf {
          _socketConfPeer   = peer
        , _socketConfSend   = encodeErgoMessage net <$> leftmost [reqE, handshakeE]
        , _socketConfPeeker = undefined
        , _socketConfClose  = closeE
        , _socketConfProxy  = proxyD
        }
    let respE = _socketInbound s
    handshakeE <- performEvent $ ffor (socketConnected s) $ const $ do
        ct <- liftIO getCurrentTime
        pure $ MsgHandshake $ makeHandshake 0 ct 
    performEvent $ ffor respE $ const $ nodeLog "some ergo message"

  proxyD <- getSocksConf

  statRef <- newExternalRef Nothing

  pure $ NodeConnection {
      nodeconCurrency   = ERGO
    , nodeconUrl        = url
    , nodeconStatus     = statRef
    , nodeconOpensE     = never
    , nodeconCloseE     = never
    , nodeconRespE      = never
    , nodeconExtra      = ()
    , nodeconIsUp       = pure False
    , nodeconDoLog      = False
    , nodeconHeight     = pure Nothing
    }
