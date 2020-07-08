module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
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
  ) where

import Control.Lens
import Control.Monad
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
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Native
import Ergvein.Wallet.Platform

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Network.Haskoin.Block as HB

class (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t m | m -> t where
  getAddressByCurIx :: Currency -> Int -> m Base58
  getEncryptedPrvStorage :: m EncryptedPrvStorage
  getWalletName          :: m Text
  getPubStorage          :: m PubStorage
  getPubStorageD         :: m (Dynamic t PubStorage)
  storeWallet            :: Event t () -> m ()
  modifyPubStorage       :: Event t (PubStorage -> Maybe PubStorage) -> m (Event t ())

setLastSeenHeight :: MonadStorage t m => Currency -> Event t BlockHeight -> m ()
setLastSeenHeight cur e = void . modifyPubStorage $ ffor e $ \h ps -> Just $
  ps & pubStorage'currencyPubStorages . at cur . _Just . currencyPubStorage'height ?~ h

addTxToPubStorage :: MonadStorage t m => Event t (TxId, EgvTx) -> m ()
addTxToPubStorage txE = void . modifyPubStorage $ ffor txE $ \(txid, etx) ps -> Just $ let
  cur = case etx of
    BtcTx{} -> BTC
    ErgTx{} -> ERGO
  in ps & pubStorage'currencyPubStorages
      . at cur . _Just                  -- TODO: Fix this part once there is a way to generate keys. Or signal an impposible situation
      . currencyPubStorage'transactions . at txid .~ Just etx
{-# INLINE addTxToPubStorage #-}

addTxMapToPubStorage :: MonadStorage t m => Event t (Currency, Map TxId EgvTx) -> m ()
addTxMapToPubStorage txmapE = void . modifyPubStorage $ ffor txmapE $ \(cur, txm) ps -> Just $
  ps & pubStorage'currencyPubStorages
    . at cur . _Just
    . currencyPubStorage'transactions %~ M.union txm

setLabelToExtPubKey :: MonadStorage t m => Event t (Currency, Int, Text) -> m ()
setLabelToExtPubKey reqE = void . modifyPubStorage $ ffor reqE $ \(cur, i, l) ->
  updateKeyBoxWith cur External i $ \kb -> kb {pubKeyBox'key = updateKeyLabel l $ pubKeyBox'key kb}

setFlagToExtPubKey :: MonadStorage t m => Event t (Currency, Int) -> m ()
setFlagToExtPubKey reqE = void . modifyPubStorage $ ffor reqE $ \(cur, i) ->
  updateKeyBoxWith cur External i $ \kb -> kb {pubKeyBox'manual = True}

insertTxsUtxoInPubKeystore :: MonadStorage t m
  => Currency
  -> Event t (V.Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
  -> m (Event  t ())
insertTxsUtxoInPubKeystore cur reqE = modifyPubStorage $ ffor reqE $ \(vec, upds) ps -> let
  txmap = M.unions $ V.toList $ snd $ V.unzip vec
  ps1 = modifyCurrStorage cur (currencyPubStorage'transactions %~ M.union txmap) ps
  ps2 = case cur of
    BTC -> updateBtcUtxoSet upds ps1
    _ -> ps1
  upd (ScanKeyBox{..}, txm) ps' = let txs = M.elems txm in updateKeyBoxWith cur scanBox'purpose scanBox'index
    (\kb -> kb {pubKeyBox'txs = S.union (pubKeyBox'txs kb) $ S.fromList (fmap egvTxId txs)}) ps'
  go !macc val = case macc of
    Nothing -> upd val ps1
    Just acc -> maybe (Just acc) Just $ upd val acc
  in V.foldl' go (Just ps2) vec

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


reconfirmBtxUtxoSet :: MonadStorage t m => Event t BlockHeight -> m ()
reconfirmBtxUtxoSet reqE = void . modifyPubStorage $ ffor reqE $ \bh ps ->
  Just $ modifyCurrStorage BTC (\cps -> cps & currencyPubStorage'utxos %~ reconfirmBtxUtxoSetPure bh) ps

getWalletsScannedHeightD :: MonadStorage t m => Currency -> m (Dynamic t BlockHeight)
getWalletsScannedHeightD cur = do
  psD <- getPubStorageD
  pure $ ffor psD $ \ps -> fromMaybe h0 $ join $ ps ^. pubStorage'currencyPubStorages . at cur
    & \mcps -> ffor mcps $ \cps -> cps ^. currencyPubStorage'scannedHeight
  where h0 = fromIntegral $ filterStartingHeight cur

writeWalletsScannedHeight :: MonadStorage t m => Event t (Currency, BlockHeight) -> m (Event t ())
writeWalletsScannedHeight reqE = modifyPubStorage $ ffor reqE $ \(cur, h) ps -> let
  mcp = ps ^. pubStorage'currencyPubStorages . at cur
  in ffor mcp $ const $ ps & pubStorage'currencyPubStorages . at cur
    %~ \mcps -> ffor mcps $ \cps -> cps & currencyPubStorage'scannedHeight .~ Just h

insertBlockHeaders :: MonadStorage t m => Currency -> Event t [HB.Block] -> m ()
insertBlockHeaders cur reqE = void . modifyPubStorage $ ffor reqE $ \blocks ps -> case blocks of
  [] -> Nothing
  _ -> let blkmap = M.fromList $ (\blk -> let bhead = HB.blockHeader blk in (HB.headerHash bhead, bhead)) <$> blocks
    in Just $ ps & pubStorage'currencyPubStorages . at cur %~ fmap (currencyPubStorage'headers %~ M.union blkmap)

getBtcUtxoD :: MonadStorage t m => m (Dynamic t BtcUtxoSet)
getBtcUtxoD = do
  pubD <- getPubStorageD
  pure $ ffor pubD $ \ps -> fromMaybe M.empty $
    ps ^. pubStorage'currencyPubStorages . at BTC & fmap (view currencyPubStorage'utxos)

addOutgoingTx :: MonadStorage t m => Event t EgvTx -> m ()
addOutgoingTx reqE =  void . modifyPubStorage $ ffor reqE $ \etx ->
  Just . modifyCurrStorage (egvTxCurrency etx) (currencyPubStorage'outgoing %~ S.insert (egvTxId etx))
