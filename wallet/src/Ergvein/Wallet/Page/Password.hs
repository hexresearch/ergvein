module Ergvein.Wallet.Page.Password(
    passwordPage
  ) where

import Ergvein.Wallet.Monad
import Ergvein.Wallet.Password

passwordPage :: MonadFront t m => m ()
passwordPage = void $ setupPassword
