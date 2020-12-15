module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  , HasPubStorage(..)
  , HasTxStorage(..)
  , getPubStorageCurD
  , getPubStorageBtcD
  , getPubStorageErgoD
  , addTxToPubStorage
  , addTxMapToPubStorage
  , setLabelToExtPubKey
  , setFlagToExtPubKey
  , updateBtcUtxoSet
  , reconfirmBtxUtxoSet
  , getBtcUtxoD
  , insertTxsUtxoInPubKeystore
  , addOutgoingTx
  , removeOutgoingTxs
  , getBtcBlockHashByTxHash
  , getTxStorage
  , getTxById
  , getBlockHeaderByHash
  , storeBlockHeadersE
  , attachNewBtcHeader
  , setScannedHeightE
  , getScannedHeightD
  , getScannedHeight
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor (void)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.Haskoin.Block (Timestamp)
import Reflex

import Ergvein.Crypto
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Storage.Currency.Public (currencyPubStorage'outgoing)
import Ergvein.Types.Storage.Currency.Public.Btc (BtcPubStorage(..), btcPubStorage'utxos, btcPubStorage'headerSeq, btcPubStorage'headers)
import Ergvein.Types.Storage.Currency.Public.Ergo (ErgoPubStorage(..))
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Native
import Ergvein.Wallet.Platform

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Network.Haskoin.Block as HB
import qualified Network.Haskoin.Transaction as HT

class (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t m | m -> t where
  getAddressByCurIx      :: Currency -> Int -> m Base58
  getEncryptedPrvStorage :: m EncryptedPrvStorage
  getWalletName          :: m Text
  getPubStorage          :: m PubStorage
  getPubStorageD         :: m (Dynamic t PubStorage)
  storeWallet            :: Text -> Event t () -> m (Event t ())
  modifyPubStorage       :: Text -> Event t (PubStorage -> Maybe PubStorage) -> m (Event t ())
  -- | Get mutex that guards writing or reading from storage file
  getStoreMutex          :: m (MVar ())
  -- | Channel that writes down given storage to disk in separate thread. First element in tuple is tracing info (caller).
  getStoreChan           :: m (TChan (Text, AuthInfo))

class MonadIO m => HasPubStorage m where
  askPubStorage :: m PubStorage

instance MonadIO m => HasPubStorage (ReaderT PubStorage m) where
  askPubStorage = ask

class MonadIO m => HasTxStorage m where
  askTxStorage :: m (M.Map TxId EgvTx)

instance MonadIO m => HasTxStorage (ReaderT (M.Map TxId EgvTx) m) where
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

getPubStorageErgoD :: MonadStorage t m => m (Dynamic t (Maybe ErgoPubStorage))
getPubStorageErgoD = do
  d <- getPubStorageCurD BTC
  pure $ ffor d $ \mv -> do
    v <- mv
    v ^? currencyPubStorage'meta . _PubStorageErgo

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
  -> Event t (V.Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
  -> m (Event  t ())
insertTxsUtxoInPubKeystore caller cur reqE = modifyPubStorage clr $ ffor reqE $ \(vec, (o,i)) ps ->
  if (V.null vec && M.null o && null i) then Nothing else let
    txmap = M.unions $ V.toList $ snd $ V.unzip vec
    ps1 = modifyCurrStorage cur (currencyPubStorage'transactions %~ M.union txmap) ps
    ps2 = case cur of
      BTC -> updateBtcUtxoSet (o,i) ps1
      _ -> ps1
    upd (ScanKeyBox{..}, txm) ps' = let txs = M.elems txm in updateKeyBoxWith cur scanBox'purpose scanBox'index
      (\kb -> kb {pubKeyBox'txs = S.union (pubKeyBox'txs kb) $ S.fromList (fmap egvTxId txs)}) ps'
    go !macc val = case macc of
      Nothing -> upd val ps1
      Just acc -> maybe (Just acc) Just $ upd val acc
    in V.foldl' go (Just ps2) vec
  where clr = caller <> ":" <> "insertTxsUtxoInPubKeystore"

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
  ErgXPubKey k _ -> ErgXPubKey k l
  BtcXPubKey k _ -> BtcXPubKey k l

reconfirmBtxUtxoSet :: MonadStorage t m => Text -> Event t BlockHeight -> m ()
reconfirmBtxUtxoSet caller reqE = void . modifyPubStorage clr $ ffor reqE $ \bh ps ->
  Just $ modifyCurrStorageBtc (btcPubStorage'utxos %~ reconfirmBtcUtxoSetPure bh) ps
  where clr = caller <> ":" <> "reconfirmBtxUtxoSet"

attachNewBtcHeader :: MonadStorage t m => Text -> Bool -> Event t (HB.BlockHeight, Timestamp, HB.BlockHash) -> m (Event t ())
attachNewBtcHeader caller updHeight reqE = modifyPubStorage clr $ ffor reqE $ \(he, ts, ha) -> modifyCurrStorageMay BTC $ \ps -> let
  heha = (he, ha)
  mhead = ps ^? currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'headerSeq
  mvec = consifNEq heha . snd =<< mhead
  mseq = ffor mvec $ \v -> (ts, ) $ if V.length v > 8 then V.init v else v
  in ffor mseq $ \s -> ps
      & currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'headerSeq .~ s
      & if updHeight
        then currencyPubStorage'chainHeight .~ fromIntegral he
        else id
  where
    clr = caller <> ":" <> "attachNewBtcHeader"
    consifNEq (he, ha) vs = if V.null vs
      then Just $ V.singleton (he, ha)
      else let (vhe, _) = V.head vs in
        if vhe > he
          then Nothing
          else if vhe == he
            then Just vs
            else Just $ V.cons (he, ha) vs

getBtcUtxoD :: MonadStorage t m => m (Dynamic t BtcUtxoSet)
getBtcUtxoD = do
  pubD <- getPubStorageD
  pure $ ffor pubD $ \ps -> fromMaybe M.empty $
    ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'utxos

addOutgoingTx :: MonadStorage t m => Text -> Event t EgvTx -> m (Event t ())
addOutgoingTx caller reqE =  modifyPubStorage clr $ ffor reqE $ \etx ->
  Just . modifyCurrStorage (egvTxCurrency etx) (currencyPubStorage'outgoing %~ S.insert (egvTxId etx))
  where clr = caller <> ":" <> "addOutgoingTx"

removeOutgoingTxs :: MonadStorage t m => Text -> Currency -> Event t [EgvTx] -> m ()
removeOutgoingTxs caller cur reqE = void . modifyPubStorage clr $ ffor reqE $ \etxs ps -> let
  remset = S.fromList $ egvTxId <$> etxs
  outs = ps ^. pubStorage'currencyPubStorages . at cur . non (error "removeOutgoingTxs: not exsisting store!") . currencyPubStorage'outgoing
  uni = S.intersection outs remset
  in if S.null uni then Nothing else Just $ modifyCurrStorage cur (currencyPubStorage'outgoing %~ flip S.difference remset) ps
  where clr = caller <> ":" <> "removeOutgoingTxs"

storeBlockHeadersE :: MonadStorage t m => Text -> Currency -> Event t [HB.Block] -> m (Event t [HB.Block])
storeBlockHeadersE caller cur reqE = do
  reqD <- holdDyn Nothing $ Just <$> reqE
  storedE <- modifyPubStorage clr $ ffor reqE $ \blks ps -> let
    mmap = if null blks then Nothing
            else Just $ M.fromList $ fmap (\b -> (HB.headerHash $ HB.blockHeader $ b, HB.blockHeader b)) blks
    in ffor mmap $ \m -> modifyCurrStorageBtc (btcPubStorage'headers %~ M.union m) ps
  pure $ attachWithMaybe (\a _ -> a) (current reqD) storedE
  where clr = caller <> ":" <> "storeBlockHeadersE"

setScannedHeightE :: MonadStorage t m => Currency -> Event t BlockHeight -> m (Event t ())
setScannedHeightE cur he = modifyPubStorage "setScannedHeightE" $ ffor he $ \h ->
  Just . modifyCurrStorage cur (currencyPubStorage'scannedHeight .~ h)

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
  pure $ join $ ps ^. pubStorage'currencyPubStorages . at BTC . non (error "getBtcBlockHashByTxHash: not exsisting store!")
    . currencyPubStorage'transactions . at th & fmap getEgvTxMeta & fmap etxMetaHash . join
  where th = hkTxHashToEgv bth

getTxStorage :: HasPubStorage m => Currency -> m (M.Map TxId EgvTx)
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
