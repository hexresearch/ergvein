module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Network.Haskoin.Transaction (OutPoint)
import Reflex

import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Native

class (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t m | m -> t where
  getAddressByCurIx :: Currency -> Int -> m Base58
  getEncryptedPrvStorage :: m EncryptedPrvStorage
  getWalletName :: m Text
  getPubStorage :: m PubStorage
  getPubStorageD :: m (Dynamic t PubStorage)
  storeWallet :: Event t () -> m ()
  addTxToPubStorage :: Event t (TxId, EgvTx) -> m ()
  addTxMapToPubStorage :: Event t (Currency, Map TxId EgvTx) -> m ()
  setLabelToExtPubKey :: Event t (Currency, Int, Text) -> m ()
  setFlagToExtPubKey :: Event t (Currency, Int) -> m ()
  insertTxsInPubKeystore :: Event t (Currency, Map Int [TxId]) -> m ()
  updateBtcUtxoSet :: Event t (BtxUtxoSet, [OutPoint]) -> m ()
