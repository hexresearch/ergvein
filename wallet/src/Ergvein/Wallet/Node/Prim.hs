{-
  Primary module. Holds typeclasses that all currencies must implement
-}
module Ergvein.Wallet.Node.Prim
  (
    JSON
  , CurrencyRep(..)
  , HasNode(..)
  ) where

import Data.Aeson
import Ergvein.Types.Currency

type JSON a = (FromJSON a, ToJSON a)

-- Type-to-value mapping
class CurrencyRep cur where
  curRep :: cur -> Currency

-- Type family for request and response
class (JSON (NodeReq cur), JSON (NodeResp cur), CurrencyRep cur) => HasNode cur where
  type NodeReq cur :: *
  type NodeResp cur :: *
