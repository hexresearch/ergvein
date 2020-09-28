{-
  Application level
-}
module Ergvein.Wallet.Node
  (
    addNodeConn
  , addMultipleConns
  , removeNodeConn
  , getNodeConn
  , initNode
  , initializeNodes
  , reinitNodes
  , requestNodeWait
  , requestRandomNode
  , btcMempoolTxInserter
  , module Ergvein.Wallet.Node.Types
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe
import Data.Time.Clock.System
import Network.Socket (SockAddr)

import Ergvein.Types
import Ergvein.Types.Derive
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.BTC
import Ergvein.Wallet.Node.ERGO
import Ergvein.Wallet.Node.Prim
import Ergvein.Wallet.Node.Types
import Ergvein.Wallet.Tx
import Ergvein.Wallet.Util

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Network.Haskoin.Transaction as HT

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

getNodeConn :: CurrencyTag t a -> SockAddr -> ConnMap t -> Maybe a
getNodeConn t url cm = M.lookup url =<< DM.lookup t cm

removeNodeConn :: forall t a . CurrencyTag t a -> SockAddr -> ConnMap t -> ConnMap t
removeNodeConn curtag url cm = DM.adjust (M.delete url) curtag cm

initNode :: MonadBaseConstr t m
  => Currency
  -> NodeReqSelector t
  -> SockAddr -> m (NodeConn t)
initNode cur sel url = case cur of
  BTC   -> fmap NodeConnBTC $ initBTCNode True url  reqE
  ERGO  -> fmap NodeConnERG $ initErgoNode url reqE
  where
    reqE = extractReq sel cur url

initializeNodes :: MonadBaseConstr t m
  => NodeReqSelector t
  -> M.Map Currency [SockAddr] -> m (ConnMap t)
initializeNodes sel urlmap = do
  let ks = M.keys urlmap
  conns <- fmap join $ flip traverse ks $ \k -> traverse (initNode k sel) $ fromMaybe [] $ M.lookup k urlmap
  pure $ addMultipleConns DM.empty conns

reinitNodes :: forall t m . MonadBaseConstr t m
  => M.Map Currency [SockAddr]  -- Map with all urls
  -> M.Map Currency Bool        -- True -- initialize or keep existing conns. False -- remove conns
  -> NodeReqSelector t          -- Request selector
  -> ConnMap t                  -- Inital map of connections
  -> m (ConnMap t)
reinitNodes urls cs sel conMap = foldlM updCurr conMap $ M.toList cs
  where
    updCurr :: MonadBaseConstr t m => ConnMap t -> (Currency, Bool) -> m (ConnMap t)
    updCurr cm (cur, b) = case cur of
      BTC -> case (DM.lookup BTCTag cm, b) of
        (Nothing, True) -> do
          let conns0 = fromMaybe [] $ M.lookup BTC urls
          conns <- flip traverse conns0 $ \u -> fmap NodeConnBTC $ initBTCNode True u $ extractReq sel BTC u
          pure $ addMultipleConns cm conns
        (Just _, False) -> pure $ DM.delete BTCTag cm
        _ -> pure cm
      ERGO -> case (DM.lookup ERGOTag cm, b) of
        (Nothing, True) -> do
          let conns0 = fromMaybe [] $ M.lookup ERGO urls
          conns <- flip traverse conns0 $ \u -> fmap NodeConnERG $ initErgoNode u $ extractReq sel ERGO u
          pure $ addMultipleConns cm conns
        (Just _, False) -> pure $ DM.delete ERGOTag cm
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
        let nodes = M.elems $ fromMaybe M.empty $ DM.lookup BTCTag cm
        mn <- randomOne nodes
        pure $ fmap (\n -> ((nodeconUrl n, req), fmap NodeRespBTC $ nodeconRespE n)) mn
      NodeReqERGO{} -> do
        let nodes = M.elems $ fromMaybe M.empty $ DM.lookup ERGOTag cm
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

btcMempoolTxInserter :: MonadFront t m => Event t HT.Tx -> m (Event t ())
btcMempoolTxInserter txE = do
  pubStorageD <- getPubStorageD
  valsE <- performFork $ ffor (current pubStorageD `attach` txE) $ \(ps, tx) -> do
    let btcps = ps ^. pubStorage'currencyPubStorages . at BTC . non (error $ "btcMempoolTxInserter: BTC storage does not exist!")
    let keys = getPublicKeys $ btcps ^. currencyPubStorage'pubKeystore
        txStore = btcps ^. currencyPubStorage'transactions
    liftIO $ flip runReaderT txStore $ do
      v <- checkAddrTx' keys tx
      u <- getUtxoUpdates Nothing keys tx
      pure (v,u)
  insertTxsUtxoInPubKeystore "btcMempoolTxInserter" BTC valsE

checkAddrTx' :: (HasTxStorage m, PlatformNatives) => V.Vector ScanKeyBox -> HT.Tx -> m (V.Vector (ScanKeyBox, M.Map TxId EgvTx))
checkAddrTx' vec tx = do
  vec' <- flip traverse vec $ \kb -> do
    b <- checkAddrTx (egvXPubKeyToEgvAddress . scanBox'key $ kb) tx
    st <- liftIO $ systemToUTCTime <$> getSystemTime
    let meta = (Just (EgvTxMeta Nothing Nothing st))
    pure $ if b then Just (kb, M.singleton th (BtcTx tx meta)) else Nothing
  pure $ V.mapMaybe id vec'
  where
    th = hkTxHashToEgv $ HT.txHash tx
