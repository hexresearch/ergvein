-- | Here will be widgets that implement interactions with the storage
module Ergvein.Wallet.Storage
  (
    withWallet
  ) where

import Control.Monad.IO.Class
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Storage.Data
import Ergvein.Wallet.Storage.Util

withWallet :: (MonadFront t m, MonadIO n) => (WalletData -> n a) -> m a
withWallet = undefined

withWalletE :: (MonadFront t m, MonadIO n) => Event t (WalletData -> n a) -> m (Event t a)
withWalletE = undefined
