{-
  Application level
-}
module Ergvein.Wallet.Node
  (
    addNodeConn
  , getNodeConn
  , getAllConnByCurrency
  , initializeNodes
  , module Ergvein.Wallet.Node.Types
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Foldable
import Servant.Client(BaseUrl)

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Node.BTC
import Ergvein.Wallet.Node.ERGO
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

initNode :: (Reflex t, TriggerEvent t m, MonadIO m) => Currency -> BaseUrl -> m (NodeConn t)
initNode cur url = case cur of
  BTC   -> fmap NodeConnBTC $ initBTCNode url
  ERGO  -> fmap NodeConnERG $ initErgoNode url

initializeNodes :: (Reflex t, TriggerEvent t m, MonadIO m) => M.Map Currency [BaseUrl] -> m (ConnMap t)
initializeNodes urlmap = do
  let ks = M.keys urlmap
  conns <- fmap join $ flip traverse ks $ \k -> traverse (initNode k) $ fromMaybe [] $ M.lookup k urlmap
  pure $ foldl' (flip addNodeConn) DM.empty conns
