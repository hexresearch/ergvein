module Ergvein.Core.Node.Manage(
    addNodeConn
  , removeNodeConn
  , getNodeConn
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

getNodeConn :: CurrencyTag t a -> SockAddr -> ConnMap t -> Maybe a
getNodeConn t url cm = M.lookup url =<< DM.lookup t cm

removeNodeConn :: forall t a . CurrencyTag t a -> SockAddr -> ConnMap t -> ConnMap t
removeNodeConn curtag url cm = DM.adjust (M.delete url) curtag cm

btcMempoolTxInserter :: MonadWallet t m => Event t HT.Tx -> m (Event t ())
btcMempoolTxInserter txE = do
  pubStorageD <- getPubStorageD
  txE' <- removeTxsReplacedByFee "btcMempoolTxInserter" txE
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
  insertedE <- insertTxsUtxoInPubKeystore "btcMempoolTxInserter" BTC $ helper <$> (fmapMaybe id valsE)
  pure $ void insertedE
  where
    helper :: ((V.Vector ScanKeyBox, EgvTx), BtcUtxoUpdate) -> (V.Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
    helper ((vec, tx), utxoUpd) = ((, M.fromList [(egvTxId tx, tx)]) <$> vec, utxoUpd)

-- | Finds all txs that should be replaced by given tx and removes them from storage.
-- Also stores information about transaction replacements in the storage.
-- Stage 1. See removeRbfTxsFromStorage1.
removeTxsReplacedByFee :: MonadStorage t m =>
  Text ->
  Event t BtcTxRaw ->
  m (Event t BtcTxRaw)
removeTxsReplacedByFee caller replacingTxE = do
  replacingTxD <- holdDyn Nothing (Just <$> replacingTxE)
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
      directlyReplacedTxs <- filterM (fmap (== Just True) . replacesByFee replacingTx) otherUnconfirmedTxs
      directlyReplacedTxsChilds <- L.concat <$> traverse getChildTxs directlyReplacedTxs
      possiblyReplacedTxs <- filterM (fmap (== Nothing) . replacesByFee replacingTx) otherUnconfirmedTxs
      possiblyReplacedTxsChilds <- L.concat <$> traverse getChildTxs possiblyReplacedTxs
      -- Also remove all txs that are in the same group in btcPubStorage'possiblyReplacedTxs
      -- with replaced txs.
      let possiblyReplacedTxsGroups = uncurry S.insert <$> M.toList possiblyReplacedTxStore

          -- Gets txs that should be replaced form btcPubStorage'possiblyReplacedTxs if provided tx has been replaced
          getTxsFromGroups :: [S.Set BtcTxId] -> BtcTxId -> S.Set BtcTxId
          getTxsFromGroups txGroups tx = S.unions $ (\txGroup -> if S.member tx txGroup then S.delete tx txGroup else S.empty) <$> txGroups

          indirectlyReplacedTxs = S.toList $ S.unions $ getTxsFromGroups possiblyReplacedTxsGroups <$> (HT.txHash <$> directlyReplacedTxs)
          replacedTxIds = S.fromList $ (HT.txHash <$> (directlyReplacedTxs ++ directlyReplacedTxsChilds)) ++ indirectlyReplacedTxs
          possiblyReplacedTxIds = S.fromList $ HT.txHash <$> (possiblyReplacedTxs ++ possiblyReplacedTxsChilds)
      pure (replacingBtcTxId, replacedTxIds, possiblyReplacedTxIds)
  removedE <- removeRbfTxsFromStorage1 clr replacedTxsE
  pure $ fmapMaybe id (tagPromptlyDyn replacingTxD removedE)
  where clr = caller <> ":" <> "removeTxsReplacedByFee"

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
