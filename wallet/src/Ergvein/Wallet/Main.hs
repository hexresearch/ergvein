module Ergvein.Wallet.Main(
    frontend
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Seed

frontend :: MonadFront t m => m ()
-- frontend = askPasswordPage
frontend = mnemonicPage
