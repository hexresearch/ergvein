module Ergvein.Wallet.Navbar (
    navbarWidget
  , navbarWidgetAndroid
  ) where

import {-# SOURCE #-} Ergvein.Wallet.Navbar.Switcher
import Ergvein.Types.Currency
import Sepulcas.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Status.Widget
import Ergvein.Wallet.Widget.Balance

navbarWidget :: MonadFront t m => Currency -> Maybe (Dynamic t (m ())) -> NavbarItem -> m ()
navbarWidget cur prevWidget activeItem = do
  when (activeItem /= NavbarReceive) balanceWidget
  divClass "navbar-3-cols" $ do
    sendE <- navbarBtn NavbarSend activeItem
    historyE <- navbarBtn NavbarHistory activeItem
    receiveE <- navbarBtn NavbarReceive activeItem
    switchNavbar cur prevWidget (leftmost [sendE, historyE, receiveE])
  where
    balanceWidget = do
      balance <- balanceTitleWidgetSimple cur
      divClass "navbar-black" $ do
        divClass "navbar-balance" $ dynText balance
        divClass "navbar-status"  $ statusBarWidget False cur

navbarBtn :: (DomBuilder t m, PostBuild t m, MonadLocalized t m) => NavbarItem -> NavbarItem-> m (Event t NavbarItem)
navbarBtn item activeItem
  | item == activeItem = (item <$) <$> spanButton "navbar-item active" item
  | item /= activeItem = (item <$) <$> spanButton "navbar-item" item
navbarBtn _ _ = pure never

data HeaderNavigationButtonType = HeaderNavigationSend | HeaderNavigationReceive

navbarWidgetAndroid :: MonadFront t m => Currency -> Maybe (Dynamic t (m ())) -> m ()
navbarWidgetAndroid cur prevWidget = divClass "navbar-black" $ do
    balance <- balanceTitleWidgetSimple cur
    divClass "navbar-balance" $ dynText balance
    divClass "navbar-status"  $ statusBarWidget False cur
    divClass "navbar-android-controls-wrapper" $ divClass "navbar-android-controls" $ do
      sendE <- divButton "navbar-android-controls-button mlr-1" $ headerNavigationButton HeaderNavigationSend
      receiveE <- divButton "navbar-android-controls-button mlr-1" $ headerNavigationButton HeaderNavigationReceive
      switchNavbar cur prevWidget (leftmost [NavbarSend <$ sendE, NavbarReceive <$ receiveE])
  where
    headerNavigationButton btnType = do
      let navIcon = case btnType of
            HeaderNavigationSend -> elClass "i" "fas fa-arrow-up fa-fw" blank
            HeaderNavigationReceive -> elClass "i" "fas fa-arrow-down fa-fw" blank
          navText = case btnType of
            HeaderNavigationSend -> HistorySend
            HeaderNavigationReceive -> HistoryReceive
      divClass "navbar-android-controls-button-icon" navIcon
      divClass "navbar-android-controls-button-label" $ localizedText navText
