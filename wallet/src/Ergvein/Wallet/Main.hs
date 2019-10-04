module Ergvein.Wallet.Main(
    frontend
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Seed

frontend :: MonadFront t m => m ()
frontend = void mnemonicWidget
