module Ergvein.Wallet.Password(
    askPasswordModal
  ) where

import Ergvein.Wallet.Monad

askPasswordModal :: MonadFront t m => m ()
