module Ergvein.Wallet.Main(
    frontend
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Env
import Reflex.Dom

frontend :: MonadFront t m => m ()
frontend = container $ do
  h2 $ text "Wallet"
