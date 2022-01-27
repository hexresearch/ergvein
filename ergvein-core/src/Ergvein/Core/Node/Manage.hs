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
  , checkAddrMempoolTx
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock.System
import Data.Traversable (for)
import Network.Socket (SockAddr)

import Ergvein.Core.Node.Btc
import Ergvein.Core.Node.Monad
import Ergvein.Core.Node.Types
import Ergvein.Core.Settings
import Ergvein.Core.Store
import Ergvein.Core.Transaction
import Ergvein.Core.Wallet
import Ergvein.Types
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Utxo.Btc
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
  BTC  -> NodeConnBtc <$> initBtcNode True url reqE
  where
    reqE = extractReq sel cur url

initializeNodes :: (MonadSettings t m)
  => NodeReqSelector t
  -> M.Map Currency [SockAddr] -> m (ConnMap t)
initializeNodes sel urlmap = do
  conns <- sequenceA [ initNode cur sel addr
                     | (cur, addrs) <- M.toList urlmap
                     , addr         <- addrs
                     ]
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
          conns <- for conns0 $ \u -> fmap NodeConnBtc $ initBtcNode True u $ extractReq sel BTC u
          pure $ addMultipleConns cm conns
        (Just _, False) -> pure $ DM.delete BtcTag cm
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
        logWrite $ nodeString nodeconCurrency nodeconUrl <> "Connection is not active. Waiting."
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
        pure $ fmap (\n -> ((nodeconUrl n, req), NodeRespBtc <$> nodeconRespE n)) mn

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
  txE' <- removeTxsReplacedByUnconfirmedTx "btcMempoolTxInserter" txE
  valsE <- performFork $ ffor (current pubStorageD `attach` txE') $ \(ps, tx) -> do
    let btcps = ps ^. btcPubStorage
        keys = getPublicKeys $ btcps ^. currencyPubStorage'pubKeystore
        txStore = btcps ^. currencyPubStorage'transactions
    liftIO $ flip runReaderT txStore $ do
      mcheckAddrTxResult <- checkAddrMempoolTx keys tx
      case mcheckAddrTxResult of
        Nothing -> pure Nothing
        Just checkAddrTxResult -> do
          utxoUpdates <- getUtxoUpdates Nothing keys tx
          pure $ Just (checkAddrTxResult, utxoUpdates)
  insertedE <- insertTxsUtxoInPubKeystore "btcMempoolTxInserter" BTC $ helper <$> fmapMaybe id valsE
  pure $ void insertedE
  where
    helper :: ((V.Vector ScanKeyBox, EgvTx), BtcUtxoUpdate) -> (V.Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
    helper ((vec, tx), utxoUpd) = ((, M.fromList [(egvTxId tx, tx)]) <$> vec, utxoUpd)

-- | Checks tx with checkAddrTx against provided keys and returns that tx in EgvTx format with matched keys vector.
-- Returns Nothing if no key has been matched
checkAddrMempoolTx :: (HasTxStorage m, PlatformNatives) => V.Vector ScanKeyBox -> HT.Tx -> m (Maybe (V.Vector ScanKeyBox, EgvTx))
checkAddrMempoolTx vec tx = do
  st <- liftIO $ systemToUTCTime <$> getSystemTime
  let meta = Just $ EgvTxMeta Nothing Nothing st
  vec' <- for vec $ \kb -> do
    b <- checkAddrTx (TxBtc $ BtcTx tx meta) (egvXPubKeyToEgvAddress . scanBox'key $ kb)
    pure $ if b then Just kb else Nothing
  let resultVec = V.mapMaybe id vec'
  pure $ if V.null resultVec
    then Nothing
    else Just (resultVec, TxBtc $ BtcTx tx meta)
