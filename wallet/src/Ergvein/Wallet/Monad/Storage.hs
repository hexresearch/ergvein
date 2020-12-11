module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  , HasPubStorage(..)
  , HasTxStorage(..)
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
  , txListToMap
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
  , getConfirmedTxs
  , getUnconfirmedTxs
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor (void)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Text (Text)
import Network.Haskoin.Block (Timestamp)
import Reflex

import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Native
import Ergvein.Wallet.Platform

import qualified Data.List as L
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
  getStoreMutex          :: m (MVar ())

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

addTxToPubStorage :: MonadStorage t m => Text -> Event t (TxId, EgvTx) -> m ()
addTxToPubStorage caller txE = void . modifyPubStorage clr $ ffor txE $ \(txid, etx) ps -> Just $ let
  cur = case etx of
    BtcTx{} -> BTC
    ErgTx{} -> ERGO
  in ps & pubStorage'currencyPubStorages
      . at cur . _Just                  -- TODO: Fix this part once there is a way to generate keys. Or signal an impposible situation
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
  -> m (Event t ())
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

-- | Removes RBF transactions from storage and updates transaction replacements info.
-- Note: that this process has two stages. This is the first stage.
-- At first stage we remove those RBF transactions for which
-- a transaction with the highest fee (a.k.a. replacing transaction) was found succesfully.
-- This stage is performed when we receive tx form mempool.
-- The second stage is performed when one of txs stored in currencyPubStorage'possiblyReplacedTxs is confirmed.
-- Information about which transactions were repaced is stored in currencyPubStorage'replacedTxs.
-- Since we are not always able to identify the transactions with the highest fee in a sequence of RBF transactions,
-- we also keep information about which of these transactions should be deleted when one of them is confirmed.
-- This information is stored in currencyPubStorage'possiblyReplacedTxs.
removeRbfTxsFromStorage1 :: MonadStorage t m => Text -> Event t (Currency, TxId, Set TxId, Set TxId) -> m (Event t ())
removeRbfTxsFromStorage1 caller txsToReplaceE = modifyPubStorage clr $ ffor txsToReplaceE $ \(cur, replacingTxId, replacedTxIds, possiblyReplacedTxIds) ps ->
  let
    ps1 = if L.null replacedTxIds
      then Nothing
      else Just $ let
        -- Removing txs from currencyPubStorage'transactions
        ps11 = ps & pubStorage'currencyPubStorages
          . at cur . _Just
          . currencyPubStorage'transactions %~ (flip M.withoutKeys $ replacedTxIds)
        -- Removing utxos from currencyPubStorage'utxos
        ps12 = modifyCurrStorage cur (\cps -> cps & currencyPubStorage'utxos %~ (M.filterWithKey (filterOutPoints replacedTxIds))) ps11
        -- Removing txids from EgvPubKeyBoxes form currencyPubStorage'pubKeystore
        ps13 = modifyCurrStorage cur (\cps -> cps & currencyPubStorage'pubKeystore %~ removeTxIdsFromEgvKeyBoxes replacedTxIds) ps12
        -- Updating currencyPubStorage'replacedTxs
        ps14 = modifyCurrStorage cur (\cps -> cps & currencyPubStorage'replacedTxs %~ updateReplacedTxsStorage replacingTxId replacedTxIds) ps13
        in ps14
    ps2 = if L.null possiblyReplacedTxIds
      then ps1
      else Just $ let
        ps1' = fromMaybe ps ps1
        in
        -- Updating currencyPubStorage'possiblyReplacedTxs
        modifyCurrStorage cur (\cps -> cps & currencyPubStorage'possiblyReplacedTxs %~ updateReplacedTxsStorage replacingTxId possiblyReplacedTxIds) ps1'
  in ps2
  where
    clr = caller <> ":" <> "removeRbfTxsFromStorage1"

data RemoveRbfTxsInfo = RemoveRbfTxsInfo {
    removeRbfTxsInfo'keyToRemoveFromPossiblyReplacedTxs :: !TxId -- ^ Map key that should be removed from currencyPubStorage'possiblyReplacedTxs.
  , removeRbfTxsInfo'replacingTx :: !TxId -- ^ Tx that replaces txs in removeRbfTxsInfo'replacedTxs.
  , removeRbfTxsInfo'replacedTxs :: !(Set TxId) -- ^ Set of txs that was replaced and should be removed from storage.
  } deriving (Eq, Show, Ord)

-- | Removes RBF transactions from storage and updates transaction replacements info.
-- Note: that this process has two stages. This is the second stage.
-- At second stage we remove those RBF transactions for which transaction with the highest fee (a.k.a. replacing transaction) wasn't found
-- before one of them was confirmed.
-- The first stage is performed when we receive tx form mempool.
removeRbfTxsFromStorage2 :: MonadStorage t m => Text -> Event t (Currency, Set (RemoveRbfTxsInfo)) -> m (Event t ())
removeRbfTxsFromStorage2 caller txsToReplaceE = modifyPubStorage clr $ ffor txsToReplaceE $ \(cur, removeRbfTxsInfoSet) ps ->
  if S.null removeRbfTxsInfoSet
    then Nothing
    else Just $ let
      keysToRemoveFromPossiblyReplacedTxs = S.map removeRbfTxsInfo'keyToRemoveFromPossiblyReplacedTxs removeRbfTxsInfoSet
      txIdsToRemove = S.unions $ S.map removeRbfTxsInfo'replacedTxs removeRbfTxsInfoSet
      replacedTxsMap = M.fromList $ (\(RemoveRbfTxsInfo a b c) -> (b, c)) <$> (S.toList removeRbfTxsInfoSet)
      -- Removing txs from currencyPubStorage'transactions
      ps11 = ps & pubStorage'currencyPubStorages
        . at cur . _Just
        . currencyPubStorage'transactions %~ (flip M.withoutKeys $ txIdsToRemove)
      -- Removing utxos from currencyPubStorage'utxos
      ps12 = modifyCurrStorage cur (\cps -> cps & currencyPubStorage'utxos %~ (M.filterWithKey (filterOutPoints txIdsToRemove))) ps11
      -- Removing txids from EgvPubKeyBoxes form currencyPubStorage'pubKeystore
      ps13 = modifyCurrStorage cur (\cps -> cps & currencyPubStorage'pubKeystore %~ removeTxIdsFromEgvKeyBoxes txIdsToRemove) ps12
      -- Updating currencyPubStorage'replacedTxs
      ps14 = modifyCurrStorage cur (\cps -> cps & currencyPubStorage'replacedTxs %~ (M.union replacedTxsMap)) ps13
      -- Removing invalid tx ids from currencyPubStorage'possiblyReplacedTxs
      ps15 = modifyCurrStorage cur (\cps -> cps & currencyPubStorage'possiblyReplacedTxs %~ (flip M.withoutKeys $ keysToRemoveFromPossiblyReplacedTxs)) ps14
      in ps15
  where
    clr = caller <> ":" <> "removeRbfTxsFromStorage2"

filterOutPoints :: Set TxId -> HT.OutPoint -> UtxoMeta -> Bool
filterOutPoints txIdSet outPoint _ = not $ S.member (hkTxHashToEgv $ HT.outPointHash outPoint) txIdSet

removeTxIdsFromEgvKeyBoxes :: Set TxId -> PubKeystore -> PubKeystore
removeTxIdsFromEgvKeyBoxes txIdsSet PubKeystore{..} =
  let updatedPubKeystore'external = fmap (removeTxIdsFromKeybox txIdsSet) pubKeystore'external
      updatedPubKeystore'internal = fmap (removeTxIdsFromKeybox txIdsSet) pubKeystore'internal
      removeTxIdsFromKeybox txIds (EgvPubKeyBox k txs m) = EgvPubKeyBox k (S.difference txs txIdsSet) m
  in PubKeystore pubKeystore'master updatedPubKeystore'external updatedPubKeystore'internal

updateReplacedTxsStorage :: TxId -> Set TxId -> Map TxId (Set TxId) -> Map TxId (Set TxId)
updateReplacedTxsStorage replacingTxId replacedTxIds replacedTxsMap = replacedTxsMap''
  where
    -- Remove all keys that are members of replacedTxIds
    replacedTxsMap' = M.filterWithKey (\k _ -> not $ k `S.member` replacedTxIds) replacedTxsMap
    -- Collect values of removed keys into one set
    txIdsReplacedByFilteredTxIds = S.unions $ S.map (flip (M.findWithDefault S.empty) $ replacedTxsMap) replacedTxIds
    -- Combine resulting set with replacedTxIds
    updatedReplacedTxIds = S.union replacedTxIds txIdsReplacedByFilteredTxIds
    replacedTxsMap'' = M.insertWith S.union replacingTxId updatedReplacedTxIds replacedTxsMap'

txListToMap :: [EgvTx] -> Map TxId EgvTx
txListToMap txList = M.fromList $ (\tx -> (egvTxId tx, tx)) <$> txList

updateBtcUtxoSet :: BtcUtxoUpdate -> PubStorage -> PubStorage
updateBtcUtxoSet upds@(o,i) ps = if (M.null o && null i) then ps else
  modifyCurrStorage BTC (\cps -> cps & currencyPubStorage'utxos %~ updateBtcUtxoSetPure upds) ps

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
  Just $ modifyCurrStorage BTC (\cps -> cps & currencyPubStorage'utxos %~ reconfirmBtxUtxoSetPure bh) ps
  where clr = caller <> ":" <> "reconfirmBtxUtxoSet"

attachNewBtcHeader :: MonadStorage t m => Text -> Bool -> Event t (HB.BlockHeight, Timestamp, HB.BlockHash) -> m (Event t ())
attachNewBtcHeader caller updHeight reqE = modifyPubStorage clr $ ffor reqE $ \(he, ts, ha) ps -> let
  heha = (he, ha)
  mvec = join $ ps ^. pubStorage'currencyPubStorages . at BTC
    & fmap (consifNEq heha . snd . _currencyPubStorage'headerSeq)
  mseq = ffor mvec $ \v -> (ts, ) $ if V.length v > 8 then V.init v else v
  in ffor mseq $ \s -> ps & pubStorage'currencyPubStorages . at BTC
    %~ \mcps -> ffor mcps $ \cps -> cps
      & currencyPubStorage'headerSeq .~ s
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
    ps ^. pubStorage'currencyPubStorages . at BTC & fmap (view currencyPubStorage'utxos)

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
    mmap = if null blks
            then Nothing
            else Just $ M.fromList $ fmap (\b -> (HB.headerHash $ HB.blockHeader $ b, HB.blockHeader b)) blks
    in ffor mmap $ \m -> modifyCurrStorage cur (currencyPubStorage'headers %~ M.union m) ps
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
  pure $ ps ^. pubStorage'currencyPubStorages . at BTC . non (error "getBtcBlockHashByTxHash: not exsisting store!")
    . currencyPubStorage'headers . at bh

getConfirmedTxs :: HasTxStorage m => m (Map TxId EgvTx)
getConfirmedTxs = do
  txs <- askTxStorage
  pure $ M.filter (isJust . (etxMetaHeight <=< getBtcTxMeta)) txs

getUnconfirmedTxs :: HasTxStorage m => m (Map TxId EgvTx)
getUnconfirmedTxs = do
  txs <- askTxStorage
  pure $ M.filter (isNothing . (etxMetaHeight <=< getBtcTxMeta)) txs