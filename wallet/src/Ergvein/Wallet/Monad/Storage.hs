module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  , setLastSeenHeight
  , addTxToPubStorage
  , addTxMapToPubStorage
  , setLabelToExtPubKey
  , setFlagToExtPubKey
  , insertTxsInPubKeystore
  , updateBtcUtxoSet
  , getWalletsScannedHeightD
  , writeWalletsScannedHeight
  , reconfirmBtxUtxoSet
  , insertBlockHeaders
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
  updateKeyBoxWith cur i $ \kb -> kb {extKeyBox'key = updateKeyLabel l $ extKeyBox'key kb}

setFlagToExtPubKey :: MonadStorage t m => Event t (Currency, Int) -> m ()
setFlagToExtPubKey reqE = void . modifyPubStorage $ ffor reqE $ \(cur, i) ->
  updateKeyBoxWith cur i $ \kb -> kb {extKeyBox'manual = True}

insertTxsInPubKeystore :: MonadStorage t m => Event t (Currency, Map Int [EgvTx]) -> m (Event t())
insertTxsInPubKeystore reqE = modifyPubStorage $ ffor reqE $ \(cur, mtx) ps -> let
  upd i txs ps = do
    let txids = fmap egvTxId txs
        txmap = M.fromList $ fmap egvTxId txs `zip` txs
    ps' <- updateKeyBoxWith cur i (\kb -> kb {extKeyBox'txs = S.union (extKeyBox'txs kb) $ S.fromList txids}) ps
    pure $ modifyCurrStorage cur (currencyPubStorage'transactions %~ M.union txmap) ps'
  go !macc i txids = case macc of
    Nothing -> upd i txids ps
    Just acc -> maybe (Just acc) Just $ upd i txids acc
  in M.foldlWithKey' go Nothing mtx

updateKeyBoxWith :: Currency -> Int -> (EgvExternalKeyBox -> EgvExternalKeyBox) -> PubStorage -> Maybe PubStorage
updateKeyBoxWith cur i f ps =
  let mk = ps ^. pubStorage'currencyPubStorages . at cur
        & \mcps -> join $ ffor mcps $ \cps -> cps ^. currencyPubStorage'pubKeystore
          & (\v -> (V.!?) (pubKeystore'external v) i)
  in ffor mk $ \kb -> let kb' = f kb
    in ps & pubStorage'currencyPubStorages . at cur
      %~ \mcps -> ffor mcps $ \cps -> cps & currencyPubStorage'pubKeystore
        %~ \pk -> pk {pubKeystore'external = (V.//) (pubKeystore'external pk) [(i, kb')]}

updateKeyLabel :: Text -> EgvXPubKey -> EgvXPubKey
updateKeyLabel l key = case key of
  ErgXPubKey k _ -> ErgXPubKey k l
  BtcXPubKey k _ -> BtcXPubKey k l

updateBtcUtxoSet :: MonadStorage t m => Event t BtcUtxoUpdate -> m ()
updateBtcUtxoSet reqE = void . modifyPubStorage $ ffor reqE $ \upds ps -> let
  mnews = ps ^. pubStorage'currencyPubStorages . at BTC
    & \mcps -> ffor mcps $ \cps -> cps ^. currencyPubStorage'utxos & getBtcUtxoSetFromStore
      & \ms -> updateBtcUtxoSetPure upds $ fromMaybe M.empty $ ms
  in ffor mnews $ \news -> ps & pubStorage'currencyPubStorages . at BTC
    %~ \mcps -> ffor mcps $ \cps -> cps & currencyPubStorage'utxos
      %~ \us -> M.insert BTC (BtcSet news) us

reconfirmBtxUtxoSet :: MonadStorage t m => Event t BlockHeight -> m ()
reconfirmBtxUtxoSet reqE = void . modifyPubStorage $ ffor reqE $ \bh ps ->
  Just $ ps & pubStorage'currencyPubStorages . at BTC
    %~ \mcps -> ffor mcps $ \cps -> cps & currencyPubStorage'utxos
      %~ \us -> foo BTC us $ \case
        BtcSet bs -> BtcSet $ reconfirmBtxUtxoSetPure bh bs
        ErgoSet v -> ErgoSet v
  where foo b c a = M.adjust a b c

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
