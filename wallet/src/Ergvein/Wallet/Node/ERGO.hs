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
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef

import Ergvein.Types.Currency
import Ergvein.Wallet.Node.Prim

instance CurrencyRep ERGOType where
  curRep _ = ERGO

-- | TODO: Change this once actual connection is implemented
instance HasNode ERGOType where
  type NodeReq ERGOType = Text
  type NodeResp ERGOType = Text
  type NodeSpecific ERGOType = ()

initErgoNode :: (Reflex t, TriggerEvent t m, MonadIO m) => SockAddr -> Event t NodeMessage -> m (NodeERG t)
initErgoNode url _ = do
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
