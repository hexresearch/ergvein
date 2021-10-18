{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Core.Node.Monad(
    MonadNodeConstr
  , NodeReqSelector
  , extractReq
  , MonadNode(..)
  , getNodeConnectionsD
  , getNodesByCurrencyD
  , getBtcNodesD
  , requestFromNode
  , postNodeMessage
  , broadcastNodeMessage
  , requestManyFromNode
  , requestBroadcast
  , sendRandomNode
  , getNodeHeightBtc
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Random
import Data.Foldable (for_)
import Data.Functor.Misc (Const2(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Word
import Ergvein.Core.Node.Types
import Ergvein.Types
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef
import Reflex.Fork
import Sepulcas.Native

import qualified Data.Dependent.Map as DM
import qualified Data.List as L
import qualified Data.Map.Strict as M

type MonadNodeConstr t (m :: * -> *) = (
    MonadHold t m
  , MonadFix m
  , PostBuild t m
  , Adjustable t m
  , MonadIO m
  , PerformEvent t m
  , Reflex t
  , TriggerEvent t m
  , PlatformNatives
  , MonadUnliftIO (Performable m)
  , MonadSample t (Performable m)
  )

type NodeReqSelector t = EventSelector t (Const2 Currency (Map SockAddr NodeMessage))

extractReq :: Reflex t => NodeReqSelector t -> Currency -> SockAddr -> Event t NodeMessage
extractReq sel c u = select (fanMap (select sel $ Const2 c)) $ Const2 u

class MonadNodeConstr t m => MonadNode t m | m -> t where
  -- | Internal method to get connection map ref
  getNodeConnRef  :: m (ExternalRef t (ConnMap t))
  -- | Get node request event
  getNodeNodeReqSelector :: m (NodeReqSelector t)
  -- | Get node request trigger
  getNodeReqFire :: m (Map Currency (Map SockAddr NodeMessage) -> IO ())

-- | Get connections map
getNodeConnectionsD :: MonadNode t m => m (Dynamic t (ConnMap t))
getNodeConnectionsD = externalRefDynamic =<< getNodeConnRef
{-# INLINE getNodeConnectionsD #-}

-- | Get nodes by currency. Basically useless, but who knows
getNodesByCurrencyD :: MonadNode t m => Currency -> m (Dynamic t (Map SockAddr (NodeConn t)))
getNodesByCurrencyD cur =
  (fmap . fmap) (fromMaybe (M.empty) . getAllConnByCurrency cur) getNodeConnectionsD
{-# INLINE getNodesByCurrencyD #-}

-- | Get Btc nodes
getBtcNodesD :: MonadNode t m => m (Dynamic t (Map SockAddr (NodeBtc t)))
getBtcNodesD =
  (fmap . fmap) (fromMaybe (M.empty) . DM.lookup BtcTag) getNodeConnectionsD
{-# INLINE getBtcNodesD #-}

-- | Send a request to a specific URL
-- It's up to the caller to ensure that the URL actually points to a correct currency node
requestFromNode :: MonadNode t m => Event t (SockAddr, NodeReqG) -> m (Event t ())
requestFromNode reqE = do
  nodeReqFire <- getNodeReqFire
  performFork $ ffor reqE $ \(u, req) ->
    let cur = getNodeReqCurrency req
    in liftIO . nodeReqFire $ M.singleton cur $ M.singleton u $ NodeMsgReq req
{-# INLINE requestFromNode #-}

postNodeMessage :: MonadNode t m => Currency -> Event t (SockAddr, NodeMessage) -> m ()
postNodeMessage cur reqE = do
  nodeReqFire <- getNodeReqFire
  performFork_ $ ffor reqE $ \(u, msg) ->
    liftIO . nodeReqFire $ M.singleton cur $ M.singleton u msg
{-# INLINE postNodeMessage #-}

broadcastNodeMessage :: MonadNode t m => Currency -> Event t NodeMessage -> m ()
broadcastNodeMessage cur reqE = do
  nodeReqFire <- getNodeReqFire
  nodeConnRef <- getNodeConnRef
  performFork_ $ ffor reqE $ \msg -> do
    reqs <- fmap ((<$) msg . fromMaybe (M.empty) . getAllConnByCurrency cur) $ readExternalRef nodeConnRef
    liftIO . nodeReqFire $ M.singleton cur reqs
{-# INLINE broadcastNodeMessage #-}

-- | Send a multiple requests a specific URL
-- It's up to the caller to ensure that the URL actually points to a correct currency node
requestManyFromNode :: MonadNode t m => Event t (SockAddr, [NodeReqG]) -> m ()
requestManyFromNode reqE = do
  nodeReqFire <- getNodeReqFire
  performFork_ $ ffor reqE $ \(u, reqs) -> for_ reqs $ \req ->
    let cur = getNodeReqCurrency req
    in liftIO . nodeReqFire $ M.singleton cur $ M.singleton u $ NodeMsgReq req
{-# INLINE requestManyFromNode #-}

-- | Send the same requests to all URLs
requestBroadcast :: MonadNode t m => Event t NodeReqG -> m (Event t ())
requestBroadcast reqE = do
  nodeReqFire <- getNodeReqFire
  nodeConnRef <- getNodeConnRef
  performFork $ ffor reqE $ \req -> do
    let cur = getNodeReqCurrency req
    reqs <- fmap ((<$) (NodeMsgReq req) . fromMaybe (M.empty) . getAllConnByCurrency cur) $ readExternalRef nodeConnRef
    liftIO . nodeReqFire $ M.singleton cur reqs

-- | Send message to random crypto node
sendRandomNode :: MonadNode t m => Event t NodeReqG -> m (Event t ())
sendRandomNode reqE = do
  nodeReqFire <- getNodeReqFire
  nodeConnRef <- getNodeConnRef
  performFork $ ffor reqE $ \req -> do
    let cur = getNodeReqCurrency req
    nodes <- fmap (maybe [] M.toList . getAllConnByCurrency cur) $ readExternalRef nodeConnRef
    mnode <- randomElem nodes
    for_ mnode $ \(addr, _) ->
      liftIO . nodeReqFire . M.singleton cur . M.singleton addr . NodeMsgReq $ req

randomElem :: MonadIO m => [a] -> m (Maybe a)
randomElem xs = case xs of
  [] -> pure Nothing
  _ -> do
    i <- liftIO $ randomRIO (0, length xs - 1)
    pure $ Just $ xs!!i

getNodeHeightBtc :: MonadNode t m => m (Dynamic t (Maybe Word32))
getNodeHeightBtc = do
  conMapD <- getBtcNodesD
  let heightD = join $ ffor conMapD $ \connMap ->
        L.foldl' (\d1 d2 -> ffor2 d1 d2 max) (pure Nothing) (nodeconHeight <$> M.elems connMap)
  holdUniqDyn heightD
