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

data NodeConn t = NodeConnBTC !(NodeBTC t) | NodeConnERG !(NodeERG t)

data CurrencyTag t a where
  BtcTag :: CurrencyTag t (NodeBTC t)
  ErgTag :: CurrencyTag t (NodeERG t)

instance GEq (CurrencyTag t) where
  geq BtcTag BtcTag  = Just Refl
  geq ErgTag ErgTag = Just Refl
  geq _      _       = Nothing

instance GCompare (CurrencyTag t) where
  gcompare BtcTag BtcTag  = GEQ
  gcompare ErgTag ErgTag = GEQ
  gcompare BtcTag ErgTag = GGT
  gcompare ErgTag BtcTag  = GLT

type ConnMap t = DMap (CurrencyTag t) (Map SockAddr)
