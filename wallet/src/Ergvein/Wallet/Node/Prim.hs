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
  , nodeString
  , getNodeReqCurrency
  -- * Command message to a node
  , NodeMessage(..)
  -- * Generalized request-response types
  , NodeReqG(..)
  , NodeRespG(..)
  -- * Currency types
  , BTCType(..)
  , NodeERG
  , NodeBTC
  , ERGOType(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef

import Ergvein.Text (showt)
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

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
  nodeconCurrency   :: !Currency
, nodeconUrl        :: !SockAddr
, nodeconStatus     :: !(ExternalRef t (Maybe NodeStatus))
, nodeconOpensE     :: !(Event t ())
, nodeconCloseE     :: !(Event t ())
, nodeconRespE      :: !(Event t (NodeResp cur))
, nodeconExtra      :: !(NodeSpecific cur)
, nodeconIsUp       :: !(Dynamic t Bool)
, nodecondoLog      :: !Bool
}

data NodeStatus = NodeStatus {
  nodestatHeight :: !BlockHeight
, nodestatLat    :: !NominalDiffTime
} deriving (Show)

-- | Type alias for a combination of hostname and port.
type HostPort = (Host, Port)

-- | Type alias for a hostname.
type Host = String

-- | Type alias for a port number.
type Port = Int

-- | Node string for logging
nodeString :: Currency -> SockAddr -> Text
nodeString cur url = "[" <> showt cur <> "]<" <> showt url <> ">: "

data BTCType = BTCType
type NodeBTC t = NodeConnection t BTCType

data ERGOType = ERGOType
type NodeERG t = NodeConnection t ERGOType

data NodeReqG = NodeReqBTC (NodeReq BTCType) | NodeReqERGO (NodeReq ERGOType)
data NodeRespG = NodeRespBTC (NodeResp BTCType) | NodeRespERGO (NodeResp ERGOType)

data NodeMessage = NodeMsgRestart | NodeMsgClose | NodeMsgReq NodeReqG

getNodeReqCurrency :: NodeReqG -> Currency
getNodeReqCurrency req = case req of
  NodeReqBTC  {} -> BTC
  NodeReqERGO {} -> ERGO
