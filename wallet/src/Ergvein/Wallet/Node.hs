{-
  Application level
-}
module Ergvein.Wallet.Node
  (
    addNodeConn
  , getNodeConn
  , getAllConnByCurrency
  , module Ergvein.Wallet.Node.Types
  ) where

import Servant.Client(BaseUrl)

import Ergvein.Wallet.Node.Types

import qualified Data.Dependent.Map as DM
import qualified Data.Map as M

addNodeConn :: NodeConn t -> ConnMap t -> ConnMap t
addNodeConn nc cm = case nc of
  NodeConnBTC conn -> let
    u = nodeconUrl conn
    in DM.insertWith M.union BTCTag (M.singleton u conn) cm
  NodeConnERG conn -> let
    u = nodeconUrl conn
    in DM.insertWith M.union ERGOTag (M.singleton u conn) cm

getNodeConn :: CurrencyTag t a -> BaseUrl -> ConnMap t -> Maybe a
getNodeConn t url cm = M.lookup url =<< DM.lookup t cm

getAllConnByCurrency :: CurrencyTag t a -> ConnMap t -> Maybe (M.Map BaseUrl a)
getAllConnByCurrency = DM.lookup
