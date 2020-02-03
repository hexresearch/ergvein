module Ergvein.Wallet.Menu.Switcher(
    switchMenu
  ) where

import Ergvein.Wallet.Menu.Types
import Ergvein.Wallet.Monad

switchMenu :: MonadFront t m => Maybe (Dynamic t (m ())) -> Event t MenuItem -> m ()
