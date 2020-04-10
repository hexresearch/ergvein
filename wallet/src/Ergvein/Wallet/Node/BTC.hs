{-
  Implementation of BTC connector
-}
module Ergvein.Wallet.Node.BTC
  (
    BTCType(..)
  ) where

import Data.Text

import Ergvein.Types.Currency
import Ergvein.Wallet.Node.Prim

data BTCType = BTCType

instance CurrencyRep BTCType where
  curRep _ = BTC

-- | TODO: Change this once actual connection is implemented
instance HasNode BTCType where
  type NodeReq BTCType = Text
  type NodeResp BTCType = Text
