module Ergvein.Wallet.Page.Currencies(
    currenciesPage
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

currenciesPage :: MonadFront t m => m ()
currenciesPage = wrapper $ do
  pure ()
