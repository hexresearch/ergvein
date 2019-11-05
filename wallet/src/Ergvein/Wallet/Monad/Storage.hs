module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  ) where

import Ergvein.Crypto
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Storage.Secure.Data
import Network.Haskoin.Address

class MonadBaseConstr t m => MonadStorage t m | m -> t where
  getAddressesByEgvXPubKey :: EgvXPubKey -> m [Address]
  getEncryptedWallet       :: m EncryptedWalletData
