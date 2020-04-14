module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  ) where

import Data.Text (Text)
import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Native
import Reflex

class (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t m | m -> t where
  getAddressByCurIx          :: Currency -> Int -> m Base58
  getEncryptedPrivateStorage :: m EncryptedPrivateStorage
  getWalletName              :: m Text
  getPublicStorage          :: m PublicStorage
  storeWallet                :: Event t () -> m ()
