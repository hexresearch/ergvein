{-
  Application level
-}
module Ergvein.Wallet.Node
  (
    addNodeConn
  , addMultipleConns
  , removeNodeConn
  , getNodeConn
  , getAllConnByCurrency
  , initNode
  , initializeNodes
  , reinitNodes
  , requestNodeWait
  , requestRandomNode
  , module Ergvein.Wallet.Node.Types
  ) where

import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Foldable
import Data.Functor.Misc
import Data.Map (Map)
import Data.Maybe
import Network.Socket (SockAddr)
import Servant.Client(BaseUrl)

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.BTC
import Ergvein.Wallet.Node.ERGO
import Ergvein.Wallet.Node.Prim
import Ergvein.Wallet.Node.Types
import Ergvein.Wallet.Util

import qualified Data.Dependent.Map as DM
import qualified Data.Map as M
import qualified Data.Set as S

addNodeConn :: NodeConn t -> ConnMap t -> ConnMap t
addNodeConn nc cm = case nc of
  NodeConnBTC conn -> let
    u = nodeconUrl conn
    in DM.insertWith M.union BtcTag (M.singleton u conn) cm
  NodeConnERG conn -> let
    u = nodeconUrl conn
    in DM.insertWith M.union ErgTag (M.singleton u conn) cm

addMultipleConns :: Foldable f => ConnMap t -> f (NodeConn t) -> ConnMap t
addMultipleConns = foldl' (flip addNodeConn)

getNodeConn :: CurrencyTag t a -> SockAddr -> ConnMap t -> Maybe a
getNodeConn t url cm = M.lookup url =<< DM.lookup t cm

getAllConnByCurrency :: Currency -> ConnMap t -> Maybe (M.Map SockAddr (NodeConn t))
getAllConnByCurrency cur cm = case cur of
  BTC  -> (fmap . fmap) NodeConnBTC $ DM.lookup BtcTag cm
  ERGO -> (fmap . fmap) NodeConnERG $ DM.lookup ErgTag cm

removeNodeConn :: forall t a . CurrencyTag t a -> SockAddr -> ConnMap t -> ConnMap t
removeNodeConn tag url cm = DM.adjust (M.delete url) tag cm

initNode :: MonadBaseConstr t m
  => Currency
  -> RequestSelector t
  -> SockAddr -> m (NodeConn t)
initNode cur sel url = case cur of
  BTC   -> fmap NodeConnBTC $ initBTCNode True url  reqE
  ERGO  -> fmap NodeConnERG $ initErgoNode url reqE
  where
    reqE = extractReq sel cur url

initializeNodes :: MonadBaseConstr t m
  => RequestSelector t
  -> M.Map Currency [SockAddr] -> m (ConnMap t)
initializeNodes sel urlmap = do
  let ks = M.keys urlmap
  conns <- fmap join $ flip traverse ks $ \k -> traverse (initNode k sel) $ fromMaybe [] $ M.lookup k urlmap
  pure $ addMultipleConns DM.empty conns

reinitNodes :: forall t m . MonadBaseConstr t m
  => M.Map Currency [SockAddr]  -- Map with all urls
  -> M.Map Currency Bool        -- True -- initialize or keep existing conns. False -- remove conns
  -> RequestSelector t          -- Request selector
  -> ConnMap t                  -- Inital map of connections
  -> m (ConnMap t)
reinitNodes urls cs sel conMap = foldlM updCurr conMap $ M.toList cs
  where
    updCurr :: MonadBaseConstr t m => ConnMap t -> (Currency, Bool) -> m (ConnMap t)
    updCurr cm (cur, b) = case cur of
      BTC -> case (DM.lookup BtcTag cm, b) of
        (Nothing, True) -> do
          let conns0 = fromMaybe [] $ M.lookup BTC urls
          conns <- flip traverse conns0 $ \u -> fmap NodeConnBTC $ initBTCNode True u $ extractReq sel BTC u
          pure $ addMultipleConns cm conns
        (Just _, False) -> pure $ DM.delete BtcTag cm
        _ -> pure cm
      ERGO -> case (DM.lookup ErgTag cm, b) of
        (Nothing, True) -> do
          let conns0 = fromMaybe [] $ M.lookup ERGO urls
          conns <- flip traverse conns0 $ \u -> fmap NodeConnERG $ initErgoNode u $ extractReq sel ERGO u
          pure $ addMultipleConns cm conns
        (Just _, False) -> pure $ DM.delete ErgTag cm
        _ -> pure cm

-- Send a request to a node. Wait until the connection is up
requestNodeWait :: (MonadFrontAuth t m, HasNode cur)
  => NodeConnection t cur -> Event t NodeReqG -> m ()
requestNodeWait NodeConnection{..} reqE = do
  reqD <- holdDyn Nothing $ Just <$> reqE
  let passValE = updated $ (,) <$> reqD <*> nodeconIsUp
  reqE' <- fmap (fmapMaybe id) $ performEvent $ ffor passValE $ \case
    (Just _, False) -> do
      when nodecondoLog $
        logWrite $ (nodeString nodeconCurrency nodeconUrl) <> "Connection is not active. Waiting."
      pure Nothing
    (Just v, True) -> pure $ Just (nodeconUrl, v)
    _ -> pure Nothing
  requestFromNode reqE'

requestRandomNode :: forall t m. (MonadFrontAuth t m) => Event t NodeReqG -> m (Event t NodeRespG)
requestRandomNode reqE = do
  conMapD <- getNodeConnectionsD
  mreqE <- performFork $ ffor reqE $ \req -> do
    cm  <- sampleDyn conMapD
    case req of
      NodeReqBTC{} -> do
        let nodes = M.elems $ fromMaybe M.empty $ DM.lookup BtcTag cm
        mn <- randomOne nodes
        pure $ fmap (\n -> ((nodeconUrl n, req), fmap NodeRespBTC $ nodeconRespE n)) mn
      NodeReqERGO{} -> do
        let nodes = M.elems $ fromMaybe M.empty $ DM.lookup ErgTag cm
        mn <- randomOne nodes
        pure $ fmap (\n -> ((nodeconUrl n, req), fmap NodeRespERGO $ nodeconRespE n)) mn
  let reqE' = fmapMaybe id mreqE
  requestFromNode $ fmap fst reqE'
  switchHold never $ fmap snd reqE'

randomOne :: MonadIO m => [a] -> m (Maybe a)
randomOne vals = case vals of
  [] -> pure Nothing
  _ -> do
    let l = length vals
    i <- liftIO $ randomRIO (0, l - 1)
    pure $ Just $ vals!!i
