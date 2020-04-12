{-
  Primary module. Holds typeclasses that all currencies must implement
-}
module Ergvein.Wallet.Node.Prim
  (
    JSON
  , CurrencyRep(..)
  , HasNode(..)
  , NodeConnection(..)
  , NodeStatus(..)
  ) where

import Data.Aeson
import Data.Time
import Reflex
import Servant.Client(BaseUrl)

import Ergvein.Types.Transaction
import Ergvein.Types.Currency

type JSON a = (FromJSON a, ToJSON a)

-- Type-to-value mapping
class CurrencyRep cur where
  curRep :: cur -> Currency

-- Type family for request and response
class (JSON (NodeReq cur), JSON (NodeResp cur), CurrencyRep cur) => HasNode cur where
  type NodeReq cur :: *
  type NodeResp cur :: *

data NodeConnection t cur = NodeConnection {
  nodeconCurrency :: Currency
, nodeconUrl      :: BaseUrl
, nodeconStatus   :: Maybe NodeStatus
, nodeconOpensE   :: Event t ()
, nodeconClosedE  :: Event t ()
, nodeconReqE     :: NodeReq cur -> IO ()
, nodeconRespE    :: Event t (NodeResp cur)
}

data NodeStatus = NodeStatus {
  nodestatHeight :: BlockHeight
, nodestatLat    :: NominalDiffTime
}
