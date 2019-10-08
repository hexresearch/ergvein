module Ergvein.Wallet.Page.Password(
    passwordPage
  ) where

import Ergvein.Wallet.Elements 
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Password

passwordPage :: MonadFront t m => m ()
passwordPage = container $ void $ setupPassword
