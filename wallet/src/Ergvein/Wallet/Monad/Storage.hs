module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  , HasPubStorage(..)
  , setLastSeenHeight
  , addTxToPubStorage
  , addTxMapToPubStorage
  , setLabelToExtPubKey
  , setFlagToExtPubKey
  , updateBtcUtxoSet
  , getWalletsScannedHeightD
  , writeWalletsScannedHeight
  , reconfirmBtxUtxoSet
  , insertBlockHeaders
  , getBtcUtxoD
  , insertTxsUtxoInPubKeystore
  , addOutgoingTx
  , removeOutgoingTxs
  , getBtcBlockHashByTxHash
  , getTxById
  , getBlockHeaderByHash
  , storeBlockHeadersE
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor (void)
import Data.Map (Map)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.Haskoin.Transaction (OutPoint)
import Reflex

import Ergvein.Crypto
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Native
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Util

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Network.Haskoin.Block as HB
import qualified Network.Haskoin.Transaction as HT

class MonadIO m => HasPubStorage m where
  askPubStorage :: m PubStorage

instance MonadIO m => HasPubStorage (ReaderT PubStorage m) where
  askPubStorage = ask

class (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t m | m -> t where
  getAddressByCurIx :: Currency -> Int -> m Base58
  getEncryptedPrvStorage :: m EncryptedPrvStorage
  getWalletName          :: m Text
  getPubStorage          :: m PubStorage
  getPubStorageD         :: m (Dynamic t PubStorage)
  storeWallet            :: Text -> Event t () -> m ()
  modifyPubStorage       :: Text -> Event t (PubStorage -> Maybe PubStorage) -> m (Event t ())
  getStoreMutex          :: m (MVar ())

-- ===========================================================================
--           MonadStorage helpers
-- ===========================================================================

setLastSeenHeight :: MonadStorage t m => Text -> Currency -> Event t BlockHeight -> m ()
setLastSeenHeight caller cur e = void . modifyPubStorage clr $ ffor e $ \h ps -> Just $
  ps & pubStorage'currencyPubStorages . at cur . _Just . currencyPubStorage'height ?~ h
  where clr = caller <> ":" <> "setLastSeenHeight"

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

getWalletsScannedHeightD :: MonadStorage t m => Currency -> m (Dynamic t BlockHeight)
getWalletsScannedHeightD cur = do
  psD <- getPubStorageD
  pure $ ffor psD $ \ps -> fromMaybe h0 $ join $ ps ^. pubStorage'currencyPubStorages . at cur
    & \mcps -> ffor mcps $ \cps -> cps ^. currencyPubStorage'scannedHeight
  where h0 = fromIntegral $ filterStartingHeight cur

writeWalletsScannedHeight :: MonadStorage t m => Text -> Event t (Currency, BlockHeight) -> m (Event t ())
writeWalletsScannedHeight caller reqE = modifyPubStorage clr $ ffor reqE $ \(cur, h) ps -> let
  mcp = ps ^. pubStorage'currencyPubStorages . at cur
  in ffor mcp $ const $ ps & pubStorage'currencyPubStorages . at cur
    %~ \mcps -> ffor mcps $ \cps -> cps & currencyPubStorage'scannedHeight .~ Just h
  where clr = caller <> ":" <> "writeWalletsScannedHeight"

insertBlockHeaders :: MonadStorage t m => Text -> Currency -> Event t [HB.Block] -> m ()
insertBlockHeaders caller cur reqE = void . modifyPubStorage clr $ ffor reqE $ \blocks ps -> case blocks of
  [] -> Nothing
  _ -> let blkmap = M.fromList $ (\blk -> let bhead = HB.blockHeader blk in (HB.headerHash bhead, bhead)) <$> blocks
    in Just $ ps & pubStorage'currencyPubStorages . at cur %~ fmap (currencyPubStorage'headers %~ M.union blkmap)
  where clr = caller <> ":" <> "insertBlockHeaders"

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
    mmap = if null blks then Nothing
            else Just $ M.fromList $ fmap (\b -> (HB.headerHash $ HB.blockHeader $ b, HB.blockHeader b)) blks
    in ffor mmap $ \m -> modifyCurrStorage cur (currencyPubStorage'headers %~ M.union m) ps
  pure $ attachWithMaybe (\a _ -> a) (current reqD) storedE
  where clr = caller <> ":" <> "storeBlockHeadersE"

-- ===========================================================================
--           HasPubStorage helpers
-- ===========================================================================

getBtcBlockHashByTxHash :: HasPubStorage m => HT.TxHash -> m (Maybe HB.BlockHash)
getBtcBlockHashByTxHash bth = do
  ps <- askPubStorage
  pure $ ps ^. pubStorage'currencyPubStorages . at BTC . non (error "getBtcBlockHashByTxHash: not exsisting store!")
    . currencyPubStorage'transactions . at th & fmap (getEgvTxMeta) & fmap etxMetaHash . join
  where th = HT.txHashToHex bth

getTxById :: HasPubStorage m => TxId -> m (Maybe EgvTx)
getTxById tid = do
  ps <- askPubStorage
  pure $ ps ^. pubStorage'currencyPubStorages . at BTC . non (error "getBtcBlockHashByTxHash: not exsisting store!")
    . currencyPubStorage'transactions . at tid

getBlockHeaderByHash :: HasPubStorage m => HB.BlockHash -> m (Maybe HB.BlockHeader)
getBlockHeaderByHash bh = do
  ps <- askPubStorage
  pure $ ps ^. pubStorage'currencyPubStorages . at BTC . non (error "getBtcBlockHashByTxHash: not exsisting store!")
    . currencyPubStorage'headers . at bh
