module Ergvein.Wallet.Navbar.Switcher (
    switchNavbar
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar.Types

switchNavbar :: MonadFront t m => Currency -> Maybe (Dynamic t (m ())) -> Event t NavbarItem -> m ()
