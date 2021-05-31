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

import Data.Text

import Control.Monad.IO.Class
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef
import Reflex.Fork
import Network.Socket (getNameInfo, NameInfoFlag(..), SockAddr(..))

import Ergvein.Types.Currency
import Ergvein.Core.Node.Types
import Ergvein.Core.Settings
import Reflex.Network
import Data.Serialize (decode, runGet, runPut)

import Ergvein.Core.Node.Socket
import Data.Ergo.Protocol.Encoder
import Ergvein.Core.Platform
import Data.Ergo.Protocol.Decoder
import Data.Ergo.Protocol.Client hiding (SocketConf, Peer, PeekerEnv)
instance CurrencyRep ErgoType where
  curRep _ = ERGO

-- | TODO: Change this once actual connection is implemented
instance HasNode ErgoType where
  type NodeReq ErgoType = Text
  type NodeResp ErgoType = Text
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
 
  peerE <- performFork $ ffor never $ const $ do
    (Just sname, Just sport) <- liftIO $ getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True url
    pure $  Peer sname sport
  proxyD <- getSocksConf
  s <- fmap switchSocket $ networkHold (pure noSocket) $ ffor peerE $ \peer -> do
    socket SocketConf {
        _socketConfPeer   = peer
      , _socketConfSend   = fmap (encodeMessage undefined) $ leftmost [reqE, never]
      , _socketConfPeeker = undefined
      , _socketConfClose  = closeE
      , _socketConfProxy  = proxyD
      }

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
