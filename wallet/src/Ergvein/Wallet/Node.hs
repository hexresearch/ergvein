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

import Ergvein.Text
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
import Ergvein.Wallet.Transaction.Util
import Ergvein.Wallet.Util

import qualified Data.Dependent.Map as DM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

initNode :: (MonadBaseConstr t m, MonadHasSettings t m)
  => Currency
  -> NodeReqSelector t
  -> SockAddr -> m (NodeConn t)
initNode cur sel url = case cur of
  BTC   -> fmap NodeConnBTC $ initBTCNode True url  reqE
  ERGO  -> fmap NodeConnERG $ initErgoNode url reqE
  where
    reqE = extractReq sel cur url

initializeNodes :: (MonadBaseConstr t m, MonadHasSettings t m)
  => NodeReqSelector t
  -> M.Map Currency [SockAddr] -> m (ConnMap t)
initializeNodes sel urlmap = do
  let ks = M.keys urlmap
  conns <- fmap join $ flip traverse ks $ \k -> traverse (initNode k sel) $ fromMaybe [] $ M.lookup k urlmap
  pure $ addMultipleConns DM.empty conns

reinitNodes :: forall t m . (MonadBaseConstr t m, MonadHasSettings t m)
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
        keys = getPublicKeys $ btcps ^. currencyPubStorage'pubKeystore
        txStore = btcps ^. currencyPubStorage'transactions
    liftIO $ flip runReaderT txStore $ do
      checkAddrTxResult <- checkAddrTx' keys tx
      utxoUpdates <- getUtxoUpdates Nothing keys tx
      pure (checkAddrTxResult, utxoUpdates)
  -- TODO: should matchedTxsE and txInsertedE be fired in sequence?
  let matchedTxsE = helper <$> valsE
      txInsertedE = fmapMaybe txInserted valsE
  insertedE <- insertTxsUtxoInPubKeystore "btcMempoolTxInserter" BTC matchedTxsE
  _ <- removeTxsReplacedByFee "btcMempoolTxInserter" BTC txInsertedE
  pure insertedE
  where
    helper :: ((V.Vector ScanKeyBox, EgvTx), BtcUtxoUpdate) -> (V.Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
    helper ((vec, tx), utxoUpd) = ((\keyBox -> (keyBox, M.fromList [(egvTxId tx, tx)])) <$> vec, utxoUpd)

    txInserted :: ((V.Vector ScanKeyBox, EgvTx), BtcUtxoUpdate) -> Maybe EgvTx
    txInserted ((vec, tx), (utxos, outPoints)) = if V.null vec && M.null utxos && null outPoints then Nothing else Just tx

-- | Finds all txs that should be replaced by given tx and removes them from storage.
-- Also stores information about transaction replacements in the storage.
-- Stage 2. See removeRbfTxsFromStorage2.
removeTxsReplacedByFee :: MonadStorage t m =>
  Text ->
  Currency ->
  Event t EgvTx ->
  m (Event t ())
removeTxsReplacedByFee caller cur replacingTxE = do
  pubStorageD <- getPubStorageD
  replacedTxsE <- performFork $ ffor (current pubStorageD `attach` replacingTxE) $ \(ps, replacingTx) -> do
    let btcps = ps ^. pubStorage'currencyPubStorages . at BTC . non (error $ "removeTxsReplacedByFee: BTC storage does not exist!")
        txStore = btcps ^. currencyPubStorage'transactions
    liftIO $ flip runReaderT txStore $ do
      unconfirmedTxs <- getUnconfirmedTxs
      let unconfirmedBtcTxs = getBtcTx <$> unconfirmedTxs
          replacingTxId = egvTxId replacingTx
          otherUnconfirmedTxs = M.elems $ M.delete replacingTxId unconfirmedBtcTxs
          replacingBtcTx = getBtcTx replacingTx
      replacedTxs <- filterM ((fmap (== Just True)) . replacesByFee replacingBtcTx) otherUnconfirmedTxs
      replacedTxsChilds <- L.concat <$> traverse getChildTxs replacedTxs
      possiblyReplacedTxs <- filterM ((fmap (== Nothing)) . replacesByFee replacingBtcTx) otherUnconfirmedTxs
      possiblyReplacedTxsChilds <- L.concat <$> traverse getChildTxs possiblyReplacedTxs
      let replacedTxIds = S.fromList $ (hkTxHashToEgv . HT.txHash) <$> (replacedTxs ++ replacedTxsChilds)
          possiblyReplacedTxIds = S.fromList $ (hkTxHashToEgv . HT.txHash) <$> (possiblyReplacedTxs ++ possiblyReplacedTxsChilds)
      pure (cur, replacingTxId, replacedTxIds, possiblyReplacedTxIds)
  removedE <- removeRbfTxsFromStorage1 "removeTxsReplacedByFee" replacedTxsE
  pure removedE

-- | Checks tx with checkAddrTx against provided keys and returns that tx in EgvTx format with matched keys vector.
checkAddrTx' :: (HasTxStorage m, PlatformNatives) => V.Vector ScanKeyBox -> HT.Tx -> m ((V.Vector (ScanKeyBox), EgvTx))
checkAddrTx' vec tx = do
  vec' <- flip traverse vec $ \kb -> do
    b <- checkAddrTx (egvXPubKeyToEgvAddress . scanBox'key $ kb) tx
    pure $ if b then Just kb else Nothing
  st <- liftIO $ systemToUTCTime <$> getSystemTime
  let meta = Just (EgvTxMeta Nothing Nothing st)
      resultVec = V.mapMaybe id vec'
  pure (resultVec, BtcTx tx meta)
