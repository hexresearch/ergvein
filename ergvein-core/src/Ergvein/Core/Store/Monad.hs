{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Core.Store.Monad(
    MonadStorageConstr
  , MonadStorage(..)
  , HasPubStorage(..)
  , HasTxStorage(..)
  , StoreWalletPriority(..)
  , StoreWalletMsg(..)
  , getPubStorageCurD
  , getPubStorageBtcD
  , addTxToPubStorage
  , addTxMapToPubStorage
  , setLabelToExtPubKey
  , setFlagToExtPubKey
  , updateBtcUtxoSet
  , reconfirmBtxUtxoSet
  , getBtcUtxoD
  , insertTxsUtxoInPubKeystore
  , insertManyTxsUtxoInPubKeystore
  , txListToMap
  , addOutgoingTx
  , removeOutgoingTxs
  , getBtcBlockHashByTxHash
  , getTxStorage
  , getTxById
  , getBlockHeaderByHash
  , storeBlockHeadersE
  , setScannedHeightE
  , getScannedHeightD
  , getScannedHeight
  , getConfirmedTxs
  , getUnconfirmedTxs
  , setSeedBackupRequired
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random.Types (MonadRandom)
import Data.Functor (void)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Storage.Currency.Public (currencyPubStorage'outgoing, currencyPubStorage'pubKeystore)
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo.Btc
import Ergvein.Types.WalletInfo
import Reflex
import Sepulcas.Native

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Network.Haskoin.Block as HB
import qualified Network.Haskoin.Transaction as HT

type MonadStorageConstr t m =
  ( PerformEvent t m
  , TriggerEvent t m
  , MonadHold t m
  , Reflex t
  , HasStoreDir m
  , MonadIO m
  , MonadUnliftIO (Performable m)
  , MonadRandom (Performable m)
  , HasStoreDir (Performable m)
  , PlatformNatives
  )

data StoreWalletPriority = StoreWalletPriorityHigh | StoreWalletPriorityLow deriving (Eq, Show)

data StoreWalletMsg = StoreWalletMsg {
    storeWalletMsg'caller :: !Text
  , storeWalletMsg'walletInfo :: !WalletInfo
  , storeWalletMsg'priority :: !StoreWalletPriority
  , storeWalletMsg'closeStoreWorker :: !Bool
}

class MonadStorageConstr t m  => MonadStorage t m | m -> t where
  getAddressByCurIx      :: Currency -> Int -> m Base58
  getEncryptedPrvStorage :: m EncryptedPrvStorage
  getWalletName          :: m Text
  getPubStorage          :: m PubStorage
  getPubStorageD         :: m (Dynamic t PubStorage)
  storeWalletNow         :: Text -> Bool -> Event t () -> m (Event t ())
  modifyPubStorage       :: Text -> Event t (PubStorage -> Maybe PubStorage) -> m (Event t ())
  -- | Get mutex that guards writing or reading from storage file
  getStoreMutex          :: m (MVar ())
  -- | Channel that writes down given storage to disk in separate thread. First element in tuple is tracing info (caller).
  getStoreChan           :: m (TChan StoreWalletMsg)

class MonadIO m => HasPubStorage m where
  askPubStorage :: m PubStorage

instance MonadIO m => HasPubStorage (ReaderT PubStorage m) where
  askPubStorage = ask

class MonadIO m => HasTxStorage m where
  askTxStorage :: m (Map TxId EgvTx)

instance MonadIO m => HasTxStorage (ReaderT (Map TxId EgvTx) m) where
  askTxStorage = ask

-- ===========================================================================
--           MonadStorage helpers
-- ===========================================================================

getPubStorageCurD :: MonadStorage t m => Currency -> m (Dynamic t (Maybe CurrencyPubStorage))
getPubStorageCurD cur = do
  d <- getPubStorageD
  pure $ ffor d $ \v -> v ^? pubStorage'currencyPubStorages . ix cur

getPubStorageBtcD :: MonadStorage t m => m (Dynamic t (Maybe BtcPubStorage))
getPubStorageBtcD = do
  d <- getPubStorageCurD BTC
  pure $ ffor d $ \mv -> do
    v <- mv
    v ^? currencyPubStorage'meta . _PubStorageBtc

addTxToPubStorage :: MonadStorage t m => Text -> Event t (TxId, EgvTx) -> m ()
addTxToPubStorage caller txE = void . modifyPubStorage clr $ ffor txE $ \(txid, etx) ps -> Just $ let
  cur = egvTxCurrency etx
  in ps & pubStorage'currencyPubStorages
      . at cur . _Just
      . currencyPubStorage'transactions . at txid .~ Just etx
  where clr = caller <> ":" <> "addTxToPubStorage"
{-# INLINE addTxToPubStorage #-}

addTxMapToPubStorage :: MonadStorage t m => Text -> Event t (Currency, Map TxId EgvTx) -> m ()
addTxMapToPubStorage caller txmapE = void . modifyPubStorage clr $ ffor txmapE $ \(cur, txm) ps -> Just $
  ps & pubStorage'currencyPubStorages
    . at cur . _Just
    . currencyPubStorage'transactions %~ M.union txm
  where clr = caller <> ":" <> "addTxMapToPubStorage"

setLabelToExtPubKey :: MonadStorage t m => Text -> Event t (Currency, Int, Text) -> m ()
setLabelToExtPubKey caller reqE = void . modifyPubStorage clr $ ffor reqE $ \(cur, i, l) ->
  updateKeyBoxWith cur External i $ \kb -> kb {pubKeyBox'key = updateKeyLabel l $ pubKeyBox'key kb}
  where clr = caller <> ":" <> "setLabelToExtPubKey"

setFlagToExtPubKey :: MonadStorage t m => Text -> Event t (Currency, Int) -> m ()
setFlagToExtPubKey caller reqE = void . modifyPubStorage clr $ ffor reqE $ \(cur, i) ->
  updateKeyBoxWith cur External i $ \kb -> kb {pubKeyBox'manual = True}
  where clr = caller <> ":" <> "setFlagToExtPubKey"

insertTxsUtxoInPubKeystore :: MonadStorage t m
  => Text -> Currency
  -> Event t (V.Vector (ScanKeyBox, Map TxId EgvTx), BtcUtxoUpdate)
  -> m (Event t (V.Vector (ScanKeyBox, Map TxId EgvTx), BtcUtxoUpdate))
insertTxsUtxoInPubKeystore caller cur reqE = do
  valD <- holdDyn (error "insertTxsUtxoInPubKeystore: impossible") reqE
  updE <- modifyPubStorage clr $ ffor reqE $ \(vec, (o,i)) ps ->
    if V.null vec && M.null o && null i then Nothing else let
    txmap = M.unions $ V.toList $ snd $ V.unzip vec
    ps1 = modifyCurrStorage cur (currencyPubStorage'transactions %~ M.union txmap) ps
    ps2 = case cur of
      BTC -> updateBtcUtxoSet (o,i) ps1

    upd ps' (ScanKeyBox{..}, txm) = fromMaybe ps' $
      let txs = M.elems txm
      in updateKeyBoxWith cur scanBox'purpose scanBox'index
            (\kb -> kb {pubKeyBox'txs = S.union (pubKeyBox'txs kb) $ S.fromList (fmap egvTxId txs)}) ps'
    ps3 = V.foldl' upd ps2 vec
    in Just ps3
  pure $ tag (current valD) updE
  where clr = caller <> ":" <> "insertTxsUtxoInPubKeystore"

insertManyTxsUtxoInPubKeystore :: MonadStorage t m
  => Text -> Currency
  -> Event t [(V.Vector (ScanKeyBox, Map TxId EgvTx), BtcUtxoUpdate)]
  -> m (Event t [(V.Vector (ScanKeyBox, Map TxId EgvTx), BtcUtxoUpdate)])
insertManyTxsUtxoInPubKeystore caller cur reqE = do
  valD <- holdDyn (error "insertTxsUtxoInPubKeystore: impossible") reqE
  updE <- modifyPubStorage clr $ ffor reqE $ \vals ps -> if null vals
    then Nothing
    else Just $ L.foldl' inserter ps vals
  pure $ tag (current valD) updE
  where
    clr = caller <> ":" <> "insertManyTxsUtxoInPubKeystore"
    inserter ps (vec, (o,i)) =
      if V.null vec && M.null o && null i then ps else let
      txmap = M.unions $ V.toList $ snd $ V.unzip vec
      -- Flip is important here as we want avoid readding transactions into storage. See #1018
      ps1 = modifyCurrStorage cur (currencyPubStorage'transactions %~ flip M.union txmap) ps
      ps2 = case cur of
        BTC -> updateBtcUtxoSet (o,i) ps1

      upd ps' (ScanKeyBox{..}, txm) = fromMaybe ps' $
        let txs = M.elems txm
        in updateKeyBoxWith cur scanBox'purpose scanBox'index
              (\kb -> kb { pubKeyBox'txs = S.union (S.fromList (fmap egvTxId txs)) $ pubKeyBox'txs kb }) ps'
      ps3 = V.foldl' upd ps2 vec
      in ps3

txListToMap :: [EgvTx] -> Map TxId EgvTx
txListToMap txList = M.fromList $ (\tx -> (egvTxId tx, tx)) <$> txList

updateBtcUtxoSet :: BtcUtxoUpdate -> PubStorage -> PubStorage
updateBtcUtxoSet upds@(o,i) ps = if M.null o && null i then ps else
  modifyCurrStorageBtc (btcPubStorage'utxos %~ updateBtcUtxoSetPure upds) ps

updateKeyBoxWith :: Currency -> KeyPurpose -> Int -> (EgvPubKeyBox -> EgvPubKeyBox) -> PubStorage -> Maybe PubStorage
updateKeyBoxWith cur kp i f ps =
  let mk = ps ^. pubStorage'currencyPubStorages . at cur
        & \mcps -> join $ ffor mcps $ \cps -> cps ^. currencyPubStorage'pubKeystore
          & (\v -> (V.!?) (keyGetter v) i)
  in ffor mk $ \kb -> let kb' = f kb
    in ps & pubStorage'currencyPubStorages . at cur
      %~ \mcps -> ffor mcps $ \cps -> cps & currencyPubStorage'pubKeystore
        %~ \pk -> case kp of
          Internal -> pk {pubKeystore'internal = (V.//) (pubKeystore'internal pk) [(i, kb')]}
          External -> pk {pubKeystore'external = (V.//) (pubKeystore'external pk) [(i, kb')]}
  where
    keyGetter = case kp of
      Internal -> pubKeystore'internal
      External -> pubKeystore'external

updateKeyLabel :: Text -> EgvXPubKey -> EgvXPubKey
updateKeyLabel l key = case key of
  BtcXPubKey k _ -> BtcXPubKey k l

reconfirmBtxUtxoSet :: MonadStorage t m => Text -> Event t BlockHeight -> m ()
reconfirmBtxUtxoSet caller reqE = void . modifyPubStorage clr $ ffor reqE $ \bh ps ->
  Just $ modifyCurrStorageBtc (btcPubStorage'utxos %~ reconfirmBtcUtxoSetPure bh) ps
  where clr = caller <> ":" <> "reconfirmBtxUtxoSet"

getBtcUtxoD :: MonadStorage t m => m (Dynamic t BtcUtxoSet)
getBtcUtxoD = do
  pubD <- getPubStorageD
  pure $ ffor pubD $ \ps -> fromMaybe M.empty $
    ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'utxos

addOutgoingTx :: MonadStorage t m => Text -> Event t EgvTx -> m (Event t ())
addOutgoingTx caller reqE =  modifyPubStorage clr $ ffor reqE $ \etx ->
  Just . modifyCurrStorage (egvTxCurrency etx) (currencyPubStorage'outgoing %~ S.insert (egvTxId etx))
  where clr = caller <> ":" <> "addOutgoingTx"

removeOutgoingTxs :: MonadStorage t m => Text -> Currency -> Event t [EgvTx] -> m (Event t ())
removeOutgoingTxs caller cur reqE = modifyPubStorage clr $ ffor reqE $ \etxs ps -> let
  remset = S.fromList $ egvTxId <$> etxs
  outs = ps ^. pubStorage'currencyPubStorages . at cur . non (error "removeOutgoingTxs: not exsisting store!") . currencyPubStorage'outgoing
  uni = S.intersection outs remset
  in if S.null uni then Nothing else Just $ modifyCurrStorage cur (currencyPubStorage'outgoing %~ flip S.difference remset) ps
  where clr = caller <> ":" <> "removeOutgoingTxs"

storeBlockHeadersE :: MonadStorage t m => Text -> Currency -> Event t [HB.Block] -> m (Event t [HB.Block])
storeBlockHeadersE caller _ reqE = do
  reqD <- holdDyn Nothing $ Just <$> reqE
  storedE <- modifyPubStorage clr $ ffor reqE $ \blks ps -> let
    mmap = if null blks
            then Nothing
            else Just $ M.fromList $ fmap (\b -> (HB.headerHash $ HB.blockHeader b, HB.blockHeader b)) blks
    in ffor mmap $ \m -> modifyCurrStorageBtc (btcPubStorage'headers %~ M.union m) ps
  pure $ attachWithMaybe const (current reqD) storedE
  where clr = caller <> ":" <> "storeBlockHeadersE"

setScannedHeightE :: MonadStorage t m => Currency -> Event t BlockHeight -> m (Event t ())
setScannedHeightE cur he = modifyPubStorage "setScannedHeightE" $ ffor he $ \h ->
  Just . modifyCurrStorage cur (currencyPubStorage'scannedHeight .~ h)

setSeedBackupRequired :: MonadStorage t m => Event t Bool -> m (Event t ())
setSeedBackupRequired e = modifyPubStorage "setSeedBackupRequired" $ ffor e $ \seedBackupRequired ps ->
  Just $ ps { _pubStorage'seedBackupRequired = seedBackupRequired }

getScannedHeightD :: MonadStorage t m => Currency -> m (Dynamic t BlockHeight)
getScannedHeightD cur = do
  psD <- getPubStorageD
  pure $ ffor psD $ \ps ->
    maybe 0 _currencyPubStorage'scannedHeight (ps ^. pubStorage'currencyPubStorages . at cur)

getScannedHeight :: MonadStorage t m => Currency -> m BlockHeight
getScannedHeight cur = do
  ps <- getPubStorage
  pure $ maybe 0 _currencyPubStorage'scannedHeight (ps
    ^. pubStorage'currencyPubStorages
    . at cur)

-- ===========================================================================
--           HasPubStorage helpers
-- ===========================================================================

getBtcBlockHashByTxHash :: HasPubStorage m => HT.TxHash -> m (Maybe HB.BlockHash)
getBtcBlockHashByTxHash bth = do
  ps <- askPubStorage
  pure $ etxMetaHash =<< (getEgvTxMeta =<< (ps ^. btcPubStorage . currencyPubStorage'transactions . at th))
  where th = hkTxHashToEgv bth

getTxStorage :: HasPubStorage m => Currency -> m (Map TxId EgvTx)
getTxStorage cur = do
  ps <- askPubStorage
  pure $ ps ^. pubStorage'currencyPubStorages . at cur . non (error $ "getTxStorage: " <> show cur <> " storage does not exist!")
    . currencyPubStorage'transactions

getTxById :: HasTxStorage m => TxId -> m (Maybe EgvTx)
getTxById tid = fmap (M.lookup tid) askTxStorage

getBlockHeaderByHash :: HasPubStorage m => HB.BlockHash -> m (Maybe HB.BlockHeader)
getBlockHeaderByHash bh = do
  ps <- askPubStorage
  pure $ ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'headers . ix bh

getTxHeight :: EgvTx -> Maybe BlockHeight
getTxHeight (TxBtc tx) = (etxMetaHeight <=< getBtcTxMeta) tx

getConfirmedTxs :: HasTxStorage m => m (Map TxId EgvTx)
getConfirmedTxs = do
  M.filter (isJust . getTxHeight) <$> askTxStorage

getUnconfirmedTxs :: HasTxStorage m => m (Map TxId EgvTx)
getUnconfirmedTxs = do
  M.filter (isNothing . getTxHeight) <$> askTxStorage

-- Orphans

instance MonadRandom m => MonadRandom (ReaderT e m) where
  getRandomBytes = lift . getRandomBytes
  {-# INLINE getRandomBytes #-}
