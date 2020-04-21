{-
  Application level
-}
module Ergvein.Wallet.Node
  (
    addNodeConn
  , getNodeConn
  , getAllConnByCurrency
  , initializeNodes
  , reinitNodes
  , requestNodeWait
  , module Ergvein.Wallet.Node.Types
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Foldable
import Servant.Client(BaseUrl)

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.BTC
import Ergvein.Wallet.Node.ERGO
import Ergvein.Wallet.Node.Prim
import Ergvein.Wallet.Node.Types

import qualified Data.Dependent.Map as DM
import qualified Data.Map as M
import qualified Data.Set as S

addNodeConn :: NodeConn t -> ConnMap t -> ConnMap t
addNodeConn nc cm = case nc of
  NodeConnBTC conn -> let
    u = nodeconUrl conn
    in DM.insertWith M.union BTCTag (M.singleton u conn) cm
  NodeConnERG conn -> let
    u = nodeconUrl conn
    in DM.insertWith M.union ERGOTag (M.singleton u conn) cm

addMultipleConns :: Foldable f => ConnMap t -> f (NodeConn t) -> ConnMap t
addMultipleConns = foldl' (flip addNodeConn)

getNodeConn :: CurrencyTag t a -> BaseUrl -> ConnMap t -> Maybe a
getNodeConn t url cm = M.lookup url =<< DM.lookup t cm

getAllConnByCurrency :: Currency -> ConnMap t -> Maybe (M.Map BaseUrl (NodeConn t))
getAllConnByCurrency cur cm = case cur of
  BTC  -> (fmap . fmap) NodeConnBTC $ DM.lookup BTCTag cm
  ERGO -> (fmap . fmap) NodeConnERG $ DM.lookup ERGOTag cm

initNode :: MonadBaseConstr t m => Currency -> BaseUrl -> m (NodeConn t)
initNode cur url = case cur of
  BTC   -> fmap NodeConnBTC $ initBTCNode url
  ERGO  -> fmap NodeConnERG $ initErgoNode url

initializeNodes :: MonadBaseConstr t m => M.Map Currency [BaseUrl] -> m (ConnMap t)
initializeNodes urlmap = do
  let ks = M.keys urlmap
  conns <- fmap join $ flip traverse ks $ \k -> traverse (initNode k) $ fromMaybe [] $ M.lookup k urlmap
  pure $ addMultipleConns DM.empty conns

reinitNodes :: MonadBaseConstr t m
  => M.Map Currency [BaseUrl]   -- Map with all urls
  -> M.Map Currency Bool        -- True -- initialize or keep existing conns. False -- remove conns
  -> ConnMap t                  -- Inital map of connections
  -> m (ConnMap t)
reinitNodes urls cs conMap = foldlM updCurr conMap $ M.toList cs
  where
    updCurr :: MonadBaseConstr t m => ConnMap t -> (Currency, Bool) -> m (ConnMap t)
    updCurr cm (cur, b) = case cur of
      BTC -> case (DM.lookup BTCTag cm, b) of
        (Nothing, True) -> do
          conns <- traverse (fmap NodeConnBTC . initBTCNode) $ fromMaybe [] $ M.lookup BTC urls
          pure $ addMultipleConns cm conns
        (Just _, False) -> pure $ DM.delete BTCTag cm
        _ -> pure cm
      ERGO -> case (DM.lookup ERGOTag cm, b) of
        (Nothing, True) -> do
          conns <- traverse (fmap NodeConnERG . initErgoNode) $ fromMaybe [] $ M.lookup ERGO urls
          pure $ addMultipleConns cm conns
        (Just _, False) -> pure $ DM.delete ERGOTag cm
        _ -> pure cm

requestNodeWait :: (MonadBaseConstr t m, HasNode cur)
  => NodeConnection t cur -> Event t (NodeReq cur) -> m ()
requestNodeWait NodeConnection{..} reqE = do
  reqD <- holdDyn Nothing $ Just <$> reqE
  let passValE = updated $ foo <$> reqD <*> nodeconShaked
  performEvent_ $ ffor passValE $ \case
    Nothing -> logWrite $ (nodeString nodeconCurrency nodeconUrl) <> "Handshake is not yet finalized. Waiting."
    Just v  -> liftIO . nodeconReqFire $ v
  where
    foo :: Maybe a -> Bool -> Maybe a
    foo ma b = case (ma,b) of
      (Just a, True) -> Just a
      _ -> Nothing
