{-
  Implementation of ERGO connector
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Node.ERGO
  (
    ERGOType(..)
  , NodeERG
  , initErgoNode
  ) where

import Data.Text

import Control.Monad.IO.Class
import Reflex
import Reflex.ExternalRef
import Servant.Client(BaseUrl)

import Ergvein.Types.Currency
import Ergvein.Wallet.Node.Prim

instance CurrencyRep ERGOType where
  curRep _ = ERGO

-- | TODO: Change this once actual connection is implemented
instance HasNode ERGOType where
  type NodeReq ERGOType = Text
  type NodeResp ERGOType = Text
  type NodeSpecific ERGOType = ()

initErgoNode :: (Reflex t, TriggerEvent t m, MonadIO m) => BaseUrl -> m (NodeERG t)
initErgoNode url = do
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
    }
