{-
  Core types for handling of a collection of node connections
-}
module Ergvein.Wallet.Node.Types
  (
    ConnMap
  , NodeConn(..)
  -- * Reexports
  , NodeConnection(..)
  , CurrencyRep(..)
  , CurrencyTag(..)
  , BTCType(..)
  , ERGOType(..)
  , NodeBTC
  , NodeERG
  , HasNode(..)
  , NodeStatus(..)
  , NodeReqG(..)
  , NodeRespG(..)
  , NodeMessage(..)
  , getAllConnByCurrency
  ) where

import Data.GADT.Compare
import Data.Time(NominalDiffTime)
import Network.Socket (SockAddr)

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Reflex.Dom

import Ergvein.Wallet.Node.BTC
import Ergvein.Wallet.Node.ERGO
import Ergvein.Wallet.Node.Prim

import Data.Map.Strict (Map)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DM

data NodeConn t = NodeConnBTC !(NodeBTC t) | NodeConnERG !(NodeERG t)

data CurrencyTag t a where
  BTCTag :: CurrencyTag t (NodeBTC t)
  ERGOTag :: CurrencyTag t (NodeERG t)

instance GEq (CurrencyTag t) where
  geq BTCTag  BTCTag  = Just Refl
  geq ERGOTag ERGOTag = Just Refl
  geq _       _       = Nothing

instance GCompare (CurrencyTag t) where
  gcompare BTCTag   BTCTag  = GEQ
  gcompare ERGOTag  ERGOTag = GEQ
  gcompare BTCTag   ERGOTag = GGT
  gcompare ERGOTag  BTCTag  = GLT

type ConnMap t = DMap (CurrencyTag t) (Map SockAddr)

getAllConnByCurrency :: Currency -> ConnMap t -> Maybe (Map SockAddr (NodeConn t))
getAllConnByCurrency cur cm = case cur of
  BTC  -> (fmap . fmap) NodeConnBTC $ DM.lookup BTCTag cm
  ERGO -> (fmap . fmap) NodeConnERG $ DM.lookup ERGOTag cm
