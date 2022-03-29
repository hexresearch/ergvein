{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Navbar(
    MainPageNavbarItem(..)
  , NetworkPageNavbarItem(..)
  , navbarWidget
  , navbarWidgetAndroid
  , networkPageNavbarWidget
  , networkPageNavbarWidgetUnauth
  ) where

import Ergvein.Wallet.Monad

data MainPageNavbarItem
  = NavbarSend
  | NavbarHistory
  | NavbarReceive

data NetworkPageNavbarItem
  = NavbarActiveIndexers
  | NavbarInactiveIndexers

navbarWidget :: MonadFront t m => Currency -> Maybe (Dynamic t (m ())) -> MainPageNavbarItem -> m ()

navbarWidgetAndroid :: MonadFront t m => Currency -> Maybe (Dynamic t (m ())) -> m ()

networkPageNavbarWidget :: MonadFront t m => Maybe (Dynamic t (m ())) -> NetworkPageNavbarItem -> m ()

networkPageNavbarWidgetUnauth :: MonadFrontBase t m => Maybe (Dynamic t (m ())) -> NetworkPageNavbarItem -> m ()
