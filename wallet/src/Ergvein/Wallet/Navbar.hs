module Ergvein.Wallet.Navbar (
    navbarWidget
  ) where

import {-# SOURCE #-} Ergvein.Wallet.Navbar.Switcher
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar.Types

type ActiveItem = NavbarItem

navbarWidget :: (MonadFront t m) => Currency -> Maybe (Dynamic t (m ())) -> ActiveItem -> m ()
navbarWidget cur prevWidget activeItem = divClass "navbar" $ do
  sendE <- navbarBtn NavbarSend activeItem
  historyE <- navbarBtn NavbarHistory activeItem
  recieveE <- navbarBtn NavbarRecieve activeItem
  switchNavbar cur prevWidget (leftmost [sendE, historyE, recieveE])

navbarBtn :: (DomBuilder t m, MonadLocalized t m) => NavbarItem -> ActiveItem-> m (Event t NavbarItem)
navbarBtn v activeItem
  | v == activeItem = (v <$) <$> spanBtn "navbar-item active" v
  | v /= activeItem = (v <$) <$> spanBtn "navbar-item" v
