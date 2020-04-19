{-
  Implementation of BTC connector
-}
module Ergvein.Wallet.Node.BTC
  (
    BTCType(..)
  , NodeBTC
  , initBTCNode
  ) where

import Data.Text

import Control.Monad.IO.Class
import Ergvein.Types.Currency
import Ergvein.Wallet.Node.Prim
import Ergvein.Wallet.Monad.Base
import Reflex
import Servant.Client(BaseUrl(..))

-- These two are for dummy stats
import Control.Monad.Random
import Ergvein.Text

-- Network
import UnliftIO hiding (atomically)
import Data.Conduit.Network (AppData, clientSettings, runGeneralTCPClient)
import Network.Socket
import Data.String.Conversions (cs)
import Control.Monad.STM
import Network.Haskoin.Network (Message, MessageHeader (..), getMessage, putMessage)

data BTCType = BTCType
type NodeBTC t = NodeConnection t BTCType

instance CurrencyRep BTCType where
  curRep _ = BTC

-- | TODO: Change this once actual connection is implemented
instance HasNode BTCType where
  type NodeReq BTCType = Text
  type NodeResp BTCType = Text

initBTCNode :: MonadBaseConstr t m => BaseUrl -> m (NodeBTC t)
initBTCNode url = do
  b  <- liftIO randomIO
  d :: Double <- liftIO $ randomRIO (0, 1.5)
  bh <- liftIO randomIO

  reqChanIn         <- liftIO $ newBroadcastTChanIO
  reqChanOut        <- liftIO $ atomically $ dupTChan reqChanIn
  (respE, fireResp) <- newTriggerEvent
  (reqE, fireReq)   <- newTriggerEvent
  performEvent_ $ (liftIO . atomically . writeTChan reqChanIn) <$> reqE

  pure $ NodeConnection {
    nodeconCurrency = BTC
  , nodeconUrl      = url
  , nodeconStatus   = if b then Nothing else Just $ NodeStatus bh (realToFrac d)
  , nodeconOpensE   = () <$ respE
  , nodeconClosedE  = never
  , nodeconReqE     = fireReq
  , nodeconRespE    = respE
  }

-- | Connect to a socket via TCP.
withConnection :: MonadUnliftIO m => BaseUrl -> (AppData -> m a) -> m a
withConnection BaseUrl{..} f =
  let cset = clientSettings baseUrlPort (cs baseUrlHost)
  in runGeneralTCPClient cset f
