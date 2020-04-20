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
  , HostPort
  , Host
  , Port
  ) where

import Data.Aeson
import Data.Serialize
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
class (CurrencyRep cur) => HasNode cur where
  type NodeReq cur :: *
  type NodeResp cur :: *
  type NodeSpecific cur :: *

data NodeConnection t cur = NodeConnection {
  nodeconCurrency :: Currency
, nodeconUrl      :: BaseUrl
, nodeconStatus   :: Maybe NodeStatus -- TODO: make this field Dynamic
, nodeconOpensE   :: Event t ()
, nodeconCloseEF  :: (Event t (), IO ())
, nodeconReqFire  :: NodeReq cur -> IO ()
, nodeconRespE    :: Event t (NodeResp cur)
, nodeExtra       :: NodeSpecific cur
}

data NodeStatus = NodeStatus {
  nodestatHeight :: BlockHeight
, nodestatLat    :: NominalDiffTime
}

-- | Type alias for a combination of hostname and port.
type HostPort = (Host, Port)

-- | Type alias for a hostname.
type Host = String

-- | Type alias for a port number.
type Port = Int
