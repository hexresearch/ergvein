module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  , addTxToPubStorage
  , addTxMapToPubStorage
  , setLabelToExtPubKey
  , setFlagToExtPubKey
  , insertTxsInPubKeystore
  ) where

import Control.Lens
import Data.Functor (void)
import Data.Map (Map)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Ergvein.Crypto
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Native
import Reflex

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

class (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t m | m -> t where
  getAddressByCurIx      :: Currency -> Int -> m Base58
  getEncryptedPrvStorage :: m EncryptedPrvStorage
  getWalletName          :: m Text
  getPubStorage          :: m PubStorage
  getPubStorageD         :: m (Dynamic t PubStorage)
  storeWallet            :: Event t () -> m ()
  modifyPubStorage       :: Event t (PubStorage -> Maybe PubStorage) -> m (Event t ())

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

insertTxsInPubKeystore :: MonadStorage t m => Event t (Currency, Map Int [TxId]) -> m (Event t())
insertTxsInPubKeystore reqE = modifyPubStorage $ ffor reqE $ \(cur, mtx) ps -> let
  upd i txids = updateKeyBoxWith cur i $ \kb -> kb {extKeyBox'txs = S.union (extKeyBox'txs kb) $ S.fromList txids}
  go !macc i txids = maybe (upd i txids ps) (upd i txids) macc
  in M.foldlWithKey' go Nothing mtx

updateKeyBoxWith :: Currency -> Int -> (EgvExternalKeyBox -> EgvExternalKeyBox) -> PubStorage -> Maybe PubStorage
updateKeyBoxWith cur i f ps =
  let mk = ps ^.
          pubStorage'currencyPubStorages
        . at cur
        & \mcps -> case mcps of
          Nothing -> Nothing
          Just cps -> cps ^. currencyPubStorage'pubKeystore
            & (\v -> (V.!?) (pubKeystore'external v) i)
  in case mk of
    Nothing -> Nothing
    Just kb -> let kb' = f kb
      in Just $ ps & pubStorage'currencyPubStorages . at cur
        %~ \mcps -> case mcps of
          Nothing -> Nothing
          Just cps -> Just $ cps & currencyPubStorage'pubKeystore
            %~ \pk -> pk {pubKeystore'external = (V.//) (pubKeystore'external pk) [(i, kb')]}

updateKeyLabel :: Text -> EgvXPubKey -> EgvXPubKey
updateKeyLabel l key = case key of
  ErgXPubKey k _ -> ErgXPubKey k l
  BtcXPubKey k _ -> BtcXPubKey k l
