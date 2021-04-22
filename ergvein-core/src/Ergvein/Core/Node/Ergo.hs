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

import Ergvein.Types.Currency
import Ergvein.Core.Node.Types

instance CurrencyRep ErgoType where
  curRep _ = ERGO

-- | TODO: Change this once actual connection is implemented
instance HasNode ErgoType where
  type NodeReq ErgoType = Text
  type NodeResp ErgoType = Text
  type NodeSpecific ErgoType = ()

initErgoNode :: (Reflex t, TriggerEvent t m, MonadIO m) => SockAddr -> Event t NodeMessage -> m (NodeErgo t)
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
    , nodecondoLog      = False
    , nodeconHeight     = pure Nothing
    }
