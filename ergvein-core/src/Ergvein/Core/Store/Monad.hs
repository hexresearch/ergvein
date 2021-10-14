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
  , removeRbfTxsFromStorage1
  , removeRbfTxsFromStorage2
  , RemoveRbfTxsInfo(..)
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
import Data.Functor (void)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
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
import Crypto.Random.Types (MonadRandom)
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
  pure $ ffor d $ \v -> v ^? pubStorage'currencyPubStorages . at cur . _Just

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
    if (V.null vec && M.null o && null i) then Nothing else let
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
      if (V.null vec && M.null o && null i) then ps else let
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

-- | Removes RBF transactions from storage and updates transaction replacements info.
-- Note: that this process has two stages. This is the first stage.
-- At first stage we remove those RBF transactions for which
-- a transaction with the highest fee (a.k.a. replacing transaction) was found succesfully.
-- This stage is performed when we receive tx from mempool or when we bump tx fee form this wallet.
-- The second stage is performed when one of txs stored in currencyPubStorage'possiblyReplacedTxs is confirmed.
-- Information about which transactions were repaced is stored in currencyPubStorage'replacedTxs.
-- Since we are not always able to identify the transactions with the highest fee in a sequence of RBF transactions,
-- we also keep information about which of these transactions should be deleted when one of them is confirmed.
-- This information is stored in currencyPubStorage'possiblyReplacedTxs.
removeRbfTxsFromStorage1 :: MonadStorage t m => Text -> Event t (BtcTxId, Set BtcTxId, Set BtcTxId) -> m (Event t ())
removeRbfTxsFromStorage1 caller txsToReplaceE = modifyPubStorage clr $ ffor txsToReplaceE $ \(replacingTxId, replacedTxIds, possiblyReplacedTxIds) ps ->
  if L.null replacedTxIds && L.null possiblyReplacedTxIds
    then Nothing
    else Just $ let
      -- Removing txs from btcPubStorage'transactions
      ps11 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'transactions %~ (flip M.withoutKeys $ replacedTxIds)) ps
      -- Removing utxos from btcPubStorage'utxos
      ps12 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'utxos %~ (M.filterWithKey (filterOutPoints replacedTxIds))) ps11
      -- Removing txids from EgvPubKeyBoxes in currencyPubStorage'pubKeystore
      ps13 = modifyCurrStorage BTC (\cps -> cps & currencyPubStorage'pubKeystore %~ removeTxIdsFromEgvKeyBoxes (S.map BtcTxHash replacedTxIds)) ps12
      -- Updating btcPubStorage'replacedTxs
      ps14 = modifyCurrStorageBtc (updateReplacedTxsStorage replacingTxId replacedTxIds) ps13
      -- Updating btcPubStorage'possiblyReplacedTxs
      ps15 = modifyCurrStorageBtc (updatePossiblyReplacedTxsStorage replacingTxId replacedTxIds possiblyReplacedTxIds) ps14
      in ps15
  where
    clr = caller <> ":" <> "removeRbfTxsFromStorage1"

updateReplacedTxsStorage :: BtcTxId -> Set BtcTxId -> BtcPubStorage -> BtcPubStorage
updateReplacedTxsStorage replacingTxId replacedTxIds btcPs =
  let
    replacedTxsMap = btcPs ^. btcPubStorage'replacedTxs
    -- Remove all replacedTxsMap keys that are members of replacedTxIds
    replacedTxsMap' = M.filterWithKey (\k _ -> not $ k `S.member` replacedTxIds) replacedTxsMap
    -- Collect values of removed replacedTxsMap keys into one set
    txIdsReplacedByFilteredTxIds = S.unions $ S.map (flip (M.findWithDefault S.empty) $ replacedTxsMap) replacedTxIds
    -- Combine into one set
    updatedReplacedTxIds = S.unions [replacedTxIds, txIdsReplacedByFilteredTxIds]
    -- Insert replacing tx with replaced txs into map
    updatedReplacedTxs = M.insertWith S.union replacingTxId updatedReplacedTxIds replacedTxsMap'
    updatedBtcPs = btcPs & btcPubStorage'replacedTxs .~ updatedReplacedTxs
  in updatedBtcPs

updatePossiblyReplacedTxsStorage :: BtcTxId -> Set BtcTxId -> Set BtcTxId -> BtcPubStorage -> BtcPubStorage
updatePossiblyReplacedTxsStorage possiblyReplacingTxId replacedTxIds possiblyReplacedTxIds btcPs =
  let
    possiblyReplacedTxsMap = btcPs ^. btcPubStorage'possiblyReplacedTxs
    -- Remove all keys that are members of possiblyReplacedTxIds
    possiblyReplacedTxsMap' = M.filterWithKey (\k _ -> not $ k `S.member` possiblyReplacedTxIds) possiblyReplacedTxsMap
    -- Collect values of removed keys into one set
    txIdsReplacedByFilteredTxIds = S.unions $ S.map (flip (M.findWithDefault S.empty) $ possiblyReplacedTxsMap) possiblyReplacedTxIds
    -- Combine resulting set with possiblyReplacedTxIds
    updatedPossiblyReplacedTxIds = S.union possiblyReplacedTxIds txIdsReplacedByFilteredTxIds
    -- Insert possibly replacing tx with possibly replaced txs into map
    possiblyReplacedTxsMap'' = M.insertWith S.union possiblyReplacingTxId updatedPossiblyReplacedTxIds possiblyReplacedTxsMap'
    -- If some txs from btcPubStorage'possiblyReplacedTxs have been replaced by replacingTxId
    -- then we need to delete the corresponding group
    updatedPossiblyReplacedTxs  = M.filterWithKey (\k v -> (not $ k `S.member` replacedTxIds) && (S.null $ S.intersection v replacedTxIds)) possiblyReplacedTxsMap''
    updatedBtcPs = btcPs & btcPubStorage'possiblyReplacedTxs .~ updatedPossiblyReplacedTxs
  in updatedBtcPs

data RemoveRbfTxsInfo = RemoveRbfTxsInfo {
    removeRbfTxsInfo'keyToRemoveFromPossiblyReplacedTxs :: !BtcTxId -- ^ Map key that should be removed from currencyPubStorage'possiblyReplacedTxs.
  , removeRbfTxsInfo'replacingTx :: !BtcTxId -- ^ Tx that replaces txs in removeRbfTxsInfo'replacedTxs.
  , removeRbfTxsInfo'replacedTxs :: !(Set BtcTxId) -- ^ Set of txs that was replaced and should be removed from storage.
  } deriving (Eq, Show, Ord)

-- | Removes RBF transactions from storage and updates transaction replacements info.
-- Note: that this process has two stages. This is the second stage.
-- At second stage we remove those RBF transactions for which transaction with the highest fee (a.k.a. replacing transaction) wasn't found
-- before one of them was confirmed.
-- The first stage is performed when we receive tx form mempool.
removeRbfTxsFromStorage2 :: MonadStorage t m => Text -> Event t (Set RemoveRbfTxsInfo) -> m (Event t ())
removeRbfTxsFromStorage2 caller txsToReplaceE = modifyPubStorage clr $ ffor txsToReplaceE $ \removeRbfTxsInfoSet ps ->
  if S.null removeRbfTxsInfoSet
    then Nothing
    else Just $ let
      keysToRemoveFromPossiblyReplacedTxs = S.map removeRbfTxsInfo'keyToRemoveFromPossiblyReplacedTxs removeRbfTxsInfoSet
      txIdsToRemove = S.unions $ S.map removeRbfTxsInfo'replacedTxs removeRbfTxsInfoSet
      replacedTxsMap = M.fromList $ (\(RemoveRbfTxsInfo _ b c) -> (b, c)) <$> (S.toList removeRbfTxsInfoSet)
      -- Removing txs from btcPubStorage'transactions
      ps11 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'transactions %~ (flip M.withoutKeys $ txIdsToRemove)) ps
      -- Removing utxos from btcPubStorage'utxos
      ps12 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'utxos %~ (M.filterWithKey (filterOutPoints txIdsToRemove))) ps11
      -- Removing txids from EgvPubKeyBoxes form currencyPubStorage'pubKeystore
      ps13 = modifyCurrStorage BTC (\cps -> cps & currencyPubStorage'pubKeystore %~ removeTxIdsFromEgvKeyBoxes (S.map BtcTxHash txIdsToRemove)) ps12
      -- Updating btcPubStorage'replacedTxs
      ps14 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'replacedTxs %~ (M.union replacedTxsMap)) ps13
      -- Removing replaced tx ids from btcPubStorage'possiblyReplacedTxs
      ps15 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'possiblyReplacedTxs %~ (flip M.withoutKeys $ keysToRemoveFromPossiblyReplacedTxs)) ps14
      in ps15
  where
    clr = caller <> ":" <> "removeRbfTxsFromStorage2"

filterOutPoints :: Set BtcTxId -> HT.OutPoint -> BtcUtxoMeta -> Bool
filterOutPoints txIdSet outPoint _ = not $ S.member (HT.outPointHash outPoint) txIdSet

removeTxIdsFromEgvKeyBoxes :: Set TxId -> PubKeystore -> PubKeystore
removeTxIdsFromEgvKeyBoxes txIdsSet PubKeystore{..} =
  let updatedPubKeystore'external = fmap removeTxIdsFromKeybox pubKeystore'external
      updatedPubKeystore'internal = fmap removeTxIdsFromKeybox pubKeystore'internal
      removeTxIdsFromKeybox (EgvPubKeyBox k txs m) = EgvPubKeyBox k (S.difference txs txIdsSet) m
  in PubKeystore pubKeystore'master updatedPubKeystore'external updatedPubKeystore'internal

txListToMap :: [EgvTx] -> Map TxId EgvTx
txListToMap txList = M.fromList $ (\tx -> (egvTxId tx, tx)) <$> txList

updateBtcUtxoSet :: BtcUtxoUpdate -> PubStorage -> PubStorage
updateBtcUtxoSet upds@(o,i) ps = if (M.null o && null i) then ps else
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
            else Just $ M.fromList $ fmap (\b -> (HB.headerHash $ HB.blockHeader $ b, HB.blockHeader b)) blks
    in ffor mmap $ \m -> modifyCurrStorageBtc (btcPubStorage'headers %~ M.union m) ps
  pure $ attachWithMaybe (\a _ -> a) (current reqD) storedE
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
    fromMaybe 0 $ ps ^. pubStorage'currencyPubStorages . at cur & (fmap _currencyPubStorage'scannedHeight)

getScannedHeight :: MonadStorage t m => Currency -> m BlockHeight
getScannedHeight cur = do
  ps <- getPubStorage
  pure $ fromMaybe 0 $ ps
    ^. pubStorage'currencyPubStorages
    . at cur
    & (fmap _currencyPubStorage'scannedHeight)


-- ===========================================================================
--           HasPubStorage helpers
-- ===========================================================================

getBtcBlockHashByTxHash :: HasPubStorage m => HT.TxHash -> m (Maybe HB.BlockHash)
getBtcBlockHashByTxHash bth = do
  ps <- askPubStorage
  pure $ join $ ps ^. btcPubStorage . currencyPubStorage'transactions . at th & fmap getEgvTxMeta & fmap etxMetaHash . join
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
  pure $ ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'headers . at bh . _Just

getTxHeight :: EgvTx -> Maybe BlockHeight
getTxHeight (TxBtc tx) = (etxMetaHeight <=< getBtcTxMeta) tx

getConfirmedTxs :: HasTxStorage m => m (Map TxId EgvTx)
getConfirmedTxs = do
  txs <- askTxStorage
  pure $ M.filter (isJust . getTxHeight) txs

getUnconfirmedTxs :: HasTxStorage m => m (Map TxId EgvTx)
getUnconfirmedTxs = do
  txs <- askTxStorage
  pure $ M.filter (isNothing . getTxHeight) txs

-- Orphans

instance MonadRandom m => MonadRandom (ReaderT e m) where
  getRandomBytes = lift . getRandomBytes
  {-# INLINE getRandomBytes #-}
