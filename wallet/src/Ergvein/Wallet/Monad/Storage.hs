module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  ) where

import Data.Text (Text)
import Ergvein.Crypto
import Control.Monad.IO.Class
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Data
import Network.Haskoin.Address
import Reflex

class (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t m | m -> t where
  getAddressByCurIx           :: Currency -> Int -> m Base58
  getEncryptedPrivateStorage  :: m EncryptedPrivateStorage
  getWalletName               :: m Text
  storeWallet                 :: Event t () -> m ()
