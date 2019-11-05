module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  ) where

import Data.Text(Text)
import Ergvein.Crypto
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Storage.Data
import Network.Haskoin.Address

class MonadBaseConstr t m => MonadStorage t m | m -> t where
  getAddressByCurIx   :: Currency -> Int -> m Base58
  getEncryptedWallet  :: m EncryptedWalletData
  getWalletName       :: m Text
