{-
  Implementation of ERGO connector
-}
module Ergvein.Wallet.Node.ERGO
  (
    ERGOType(..)
  , NodeERG
  , initErgoNode
  ) where

import Data.Text

import Control.Monad.IO.Class
import Reflex
import Servant.Client(BaseUrl)

import Ergvein.Types.Currency
import Ergvein.Wallet.Node.Prim

data ERGOType = ERGOType
type NodeERG t = NodeConnection t ERGOType

instance CurrencyRep ERGOType where
  curRep _ = ERGO

-- | TODO: Change this once actual connection is implemented
instance HasNode ERGOType where
  type NodeReq ERGOType = Text
  type NodeResp ERGOType = Text
  type NodeSpecific ERGOType = ()

initErgoNode :: (Reflex t, TriggerEvent t m, MonadIO m) => BaseUrl -> m (NodeERG t)
initErgoNode url =  pure $ NodeConnection {
    nodeconCurrency = ERGO
  , nodeconUrl      = url
  , nodeconStatus   = Nothing
  , nodeconOpensE   = never
  , nodeconCloseEF  = (never, pure ())
  , nodeconReqFire  = const $ pure ()
  , nodeconRespE    = never
  , nodeExtra       = ()
  }
