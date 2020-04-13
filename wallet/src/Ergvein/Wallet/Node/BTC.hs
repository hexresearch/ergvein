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
import Reflex
import Servant.Client(BaseUrl)

-- These two are for dummy stats
import Control.Monad.Random
import Ergvein.Text

data BTCType = BTCType
type NodeBTC t = NodeConnection t BTCType

instance CurrencyRep BTCType where
  curRep _ = BTC

-- | TODO: Change this once actual connection is implemented
instance HasNode BTCType where
  type NodeReq BTCType = Text
  type NodeResp BTCType = Text

initBTCNode :: (Reflex t, TriggerEvent t m, MonadIO m) => BaseUrl -> m (NodeBTC t)
initBTCNode url = do
  b  <- liftIO randomIO
  d :: Double <- liftIO $ randomRIO (0, 1.5)
  bh <- liftIO randomIO
  pure $ NodeConnection {
    nodeconCurrency = BTC
  , nodeconUrl      = url
  , nodeconStatus   = if b then Nothing else Just $ NodeStatus bh (realToFrac d)
  , nodeconOpensE   = never
  , nodeconClosedE  = never
  , nodeconReqE     = const $ pure ()
  , nodeconRespE    = never
  }
