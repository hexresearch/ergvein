module Ergvein.Core.Node.Manage(
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
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock.System
import Ergvein.Core.Node.Btc
import Ergvein.Core.Node.Ergo
import Ergvein.Core.Node.Monad
import Ergvein.Core.Node.Types
import Ergvein.Core.Settings
import Ergvein.Core.Store
import Ergvein.Core.Transaction
import Ergvein.Core.Wallet
import Ergvein.Types
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Utxo.Btc
import Network.Socket (SockAddr)
import Reflex.Flunky
import Reflex.Fork
import Sepulcas.Native

import qualified Data.Dependent.Map as DM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Network.Haskoin.Transaction as HT

addNodeConn :: NodeConn t -> ConnMap t -> ConnMap t
addNodeConn nc cm = case nc of
  NodeConnBtc conn -> let
    u = nodeconUrl conn
    in DM.insertWith M.union BtcTag (M.singleton u conn) cm
  NodeConnErgo conn -> let
    u = nodeconUrl conn
    in DM.insertWith M.union ErgoTag (M.singleton u conn) cm

addMultipleConns :: Foldable f => ConnMap t -> f (NodeConn t) -> ConnMap t
addMultipleConns = foldl' (flip addNodeConn)

getNodeConn :: CurrencyTag t a -> SockAddr -> ConnMap t -> Maybe a
getNodeConn t url cm = M.lookup url =<< DM.lookup t cm

removeNodeConn :: forall t a . CurrencyTag t a -> SockAddr -> ConnMap t -> ConnMap t
removeNodeConn curtag url cm = DM.adjust (M.delete url) curtag cm

initNode :: (MonadSettings t m)
  => Currency
  -> NodeReqSelector t
  -> SockAddr -> m (NodeConn t)
initNode cur sel url = case cur of
  BTC   -> fmap NodeConnBtc $ initBtcNode True url  reqE
  ERGO  -> fmap NodeConnErgo $ initErgoNode url reqE
  where
    reqE = extractReq sel cur url

initializeNodes :: (MonadSettings t m)
  => NodeReqSelector t
  -> M.Map Currency [SockAddr] -> m (ConnMap t)
initializeNodes sel urlmap = do
  let ks = M.keys urlmap
  conns <- fmap join $ flip traverse ks $ \k -> traverse (initNode k sel) $ fromMaybe [] $ M.lookup k urlmap
  pure $ addMultipleConns DM.empty conns

reinitNodes :: forall t m . (MonadSettings t m)
  => M.Map Currency [SockAddr]  -- Map with all urls
  -> M.Map Currency Bool        -- True -- initialize or keep existing conns. False -- remove conns
  -> NodeReqSelector t          -- Request selector
  -> ConnMap t                  -- Inital map of connections
  -> m (ConnMap t)
reinitNodes urls cs sel conMap = foldlM updCurr conMap $ M.toList cs
  where
    updCurr :: ConnMap t -> (Currency, Bool) -> m (ConnMap t)
    updCurr cm (cur, b) = case cur of
      BTC -> case (DM.lookup BtcTag cm, b) of
        (Nothing, True) -> do
          let conns0 = fromMaybe [] $ M.lookup BTC urls
          conns <- flip traverse conns0 $ \u -> fmap NodeConnBtc $ initBtcNode True u $ extractReq sel BTC u
          pure $ addMultipleConns cm conns
        (Just _, False) -> pure $ DM.delete BtcTag cm
        _ -> pure cm
      ERGO -> case (DM.lookup ErgoTag cm, b) of
        (Nothing, True) -> do
          let conns0 = fromMaybe [] $ M.lookup ERGO urls
          conns <- flip traverse conns0 $ \u -> fmap NodeConnErgo $ initErgoNode u $ extractReq sel ERGO u
          pure $ addMultipleConns cm conns
        (Just _, False) -> pure $ DM.delete ErgoTag cm
        _ -> pure cm

-- Send a request to a node. Wait until the connection is up
requestNodeWait :: (MonadNode t m, HasNode cur)
  => NodeConnection t cur -> Event t NodeReqG -> m (Event t ())
requestNodeWait NodeConnection{..} reqE = do
  reqD <- holdDyn Nothing $ Just <$> reqE
  let passValE = updated $ (,) <$> reqD <*> nodeconIsUp
  reqE' <- fmap (fmapMaybe id) $ performEvent $ ffor passValE $ \case
    (Just _, False) -> do
      when nodeconDoLog $
        logWrite $ (nodeString nodeconCurrency nodeconUrl) <> "Connection is not active. Waiting."
      pure Nothing
    (Just v, True) -> pure $ Just (nodeconUrl, v)
    _ -> pure Nothing
  requestFromNode reqE'

requestRandomNode :: forall t m. (MonadNode t m) => Event t NodeReqG -> m (Event t NodeRespG)
requestRandomNode reqE = do
  conMapD <- getNodeConnectionsD
  mreqE <- performFork $ ffor reqE $ \req -> do
    cm  <- sampleDyn conMapD
    case req of
      NodeReqBtc{} -> do
        let nodes = M.elems $ fromMaybe M.empty $ DM.lookup BtcTag cm
        mn <- randomOne nodes
        pure $ fmap (\n -> ((nodeconUrl n, req), fmap NodeRespBtc $ nodeconRespE n)) mn
      NodeReqErgo{} -> do
        let nodes = M.elems $ fromMaybe M.empty $ DM.lookup ErgoTag cm
        mn <- randomOne nodes
        pure $ fmap (\n -> ((nodeconUrl n, req), fmap NodeRespErgo $ nodeconRespE n)) mn
  let reqE' = fmapMaybe id mreqE
  _ <- requestFromNode $ fmap fst reqE'
  switchHold never $ fmap snd reqE'

randomOne :: MonadIO m => [a] -> m (Maybe a)
randomOne vals = case vals of
  [] -> pure Nothing
  _ -> do
    let l = length vals
    i <- liftIO $ randomRIO (0, l - 1)
    pure $ Just $ vals!!i

btcMempoolTxInserter :: MonadWallet t m => Event t HT.Tx -> m (Event t ())
btcMempoolTxInserter txE = do
  pubStorageD <- getPubStorageD
  valsE <- performFork $ ffor (current pubStorageD `attach` txE) $ \(ps, tx) -> do
    let btcps = ps ^. btcPubStorage
        keys = getPublicKeys $ btcps ^. currencyPubStorage'pubKeystore
        txStore = btcps ^. currencyPubStorage'transactions
    liftIO $ flip runReaderT txStore $ do
      checkAddrTxResult <- checkAddrTx' keys tx
      utxoUpdates <- getUtxoUpdates Nothing keys tx
      pure $ Just (checkAddrTxResult, utxoUpdates)
  let txInsertedE = fmapMaybe txInserted valsE
  removedE <- removeTxsReplacedByFee "btcMempoolTxInserter" txInsertedE
  matchedTxsD <- holdDyn Nothing valsE
  let matchedTxsE = attachPromptlyDynWithMaybe (\dynVal _ -> helper <$> dynVal) matchedTxsD removedE
  insertedE <- insertTxsUtxoInPubKeystore "btcMempoolTxInserter" BTC matchedTxsE
  pure insertedE
  where
    helper :: ((V.Vector ScanKeyBox, EgvTx), BtcUtxoUpdate) -> (V.Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
    helper ((vec, tx), utxoUpd) = ((\keyBox -> (keyBox, M.fromList [(egvTxId tx, tx)])) <$> vec, utxoUpd)

    txInserted :: Maybe ((V.Vector ScanKeyBox, EgvTx), BtcUtxoUpdate) -> Maybe BtcTxRaw
    txInserted Nothing = Nothing
    txInserted (Just ((vec, tx), (utxos, outPoints))) = if V.null vec && M.null utxos && null outPoints
      then Nothing
      else case tx of
        TxBtc (BtcTx btcTx _) -> Just btcTx
        _ -> Nothing

-- | Finds all txs that should be replaced by given tx and removes them from storage.
-- Also stores information about transaction replacements in the storage.
-- Stage 1. See removeRbfTxsFromStorage1.
removeTxsReplacedByFee :: MonadStorage t m =>
  Text ->
  Event t BtcTxRaw ->
  m (Event t ())
removeTxsReplacedByFee caller replacingTxE = do
  pubStorageD <- getPubStorageD
  replacedTxsE <- performFork $ ffor (current pubStorageD `attach` replacingTxE) $ \(ps, replacingTx) -> do
    let btcps = ps ^. btcPubStorage
        txStore = btcps ^. currencyPubStorage'transactions
        possiblyReplacedTxStore = fromMaybe M.empty $ btcps ^? currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'possiblyReplacedTxs
    liftIO $ flip runReaderT txStore $ do
      unconfirmedTxs <- getUnconfirmedTxs
      let unconfirmedBtcTxs = getBtcTx . fromJust . toTxBtc <$> unconfirmedTxs
          replacingBtcTxId = HT.txHash replacingTx
          replacingTxId = BtcTxHash replacingBtcTxId
          otherUnconfirmedTxs = M.elems $ M.delete replacingTxId unconfirmedBtcTxs
      directlyReplacedTxs <- filterM ((fmap (== Just True)) . replacesByFee replacingTx) otherUnconfirmedTxs
      directlyReplacedTxsChilds <- L.concat <$> traverse getChildTxs directlyReplacedTxs
      possiblyReplacedTxs <- filterM ((fmap (== Nothing)) . replacesByFee replacingTx) otherUnconfirmedTxs
      possiblyReplacedTxsChilds <- L.concat <$> traverse getChildTxs possiblyReplacedTxs
      -- Also remove all txs that are in the same group in btcPubStorage'possiblyReplacedTxs
      -- with replaced txs.
      let possiblyReplacedTxsGroups = (\(k, v) -> S.insert k v) <$> M.toList possiblyReplacedTxStore

          -- Gets txs that should be replaced form btcPubStorage'possiblyReplacedTxs if provided tx has been replaced
          getTxsFromGroups :: [S.Set BtcTxId] -> BtcTxId -> S.Set BtcTxId
          getTxsFromGroups txGroups tx = S.unions $ (\txGroup -> if S.member tx txGroup then S.delete tx txGroup else S.empty) <$> txGroups

          indirectlyReplacedTxs = S.toList $ S.unions $ (getTxsFromGroups possiblyReplacedTxsGroups) <$> (HT.txHash <$> directlyReplacedTxs)
          replacedTxIds = S.fromList $ (HT.txHash <$> (directlyReplacedTxs ++ directlyReplacedTxsChilds)) ++ indirectlyReplacedTxs
          possiblyReplacedTxIds = S.fromList $ HT.txHash <$> (possiblyReplacedTxs ++ possiblyReplacedTxsChilds)
      pure (replacingBtcTxId, replacedTxIds, possiblyReplacedTxIds)
  removedE <- removeRbfTxsFromStorage1 clr replacedTxsE
  pure removedE
  where clr = caller <> ":" <> "removeTxsReplacedByFee"

-- | Checks tx with checkAddrTx against provided keys and returns that tx in EgvTx format with matched keys vector.
checkAddrTx' :: (HasTxStorage m, PlatformNatives) => V.Vector ScanKeyBox -> HT.Tx -> m ((V.Vector (ScanKeyBox), EgvTx))
checkAddrTx' vec tx = do
  st <- liftIO $ systemToUTCTime <$> getSystemTime
  let meta = (Just (EgvTxMeta Nothing Nothing st))
  vec' <- flip traverse vec $ \kb -> do
    b <- checkAddrTx (TxBtc $ BtcTx tx meta) (egvXPubKeyToEgvAddress . scanBox'key $ kb)
    pure $ if b then Just kb else Nothing
  let resultVec = V.mapMaybe id vec'
  pure (resultVec, TxBtc $ BtcTx tx meta)
