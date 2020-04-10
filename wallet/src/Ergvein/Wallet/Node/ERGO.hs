{-
  Implementation of ERGO connector
-}
module Ergvein.Wallet.Node.ERGO
  (
    ERGOType(..)
  ) where

import Data.Text

import Ergvein.Types.Currency
import Ergvein.Wallet.Node.Prim

data ERGOType = ERGOType

instance CurrencyRep ERGOType where
  curRep _ = ERGO

-- | TODO: Change this once actual connection is implemented
instance HasNode ERGOType where
  type NodeReq ERGOType = Text
  type NodeResp ERGOType = Text
