{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Navbar(
    MainPageNavbarItem(..)
  , NetworkPageNavbarItem(..)
  , navbarWidget
  , navbarWidgetAndroid
  , networkPageNavbarWidget
  , networkPageNavbarWidgetUnauth
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.Receive
import Ergvein.Wallet.Page.Send.Btc
import Ergvein.Wallet.Page.Settings.Network
import Ergvein.Wallet.Status.Widget
import Ergvein.Wallet.Widget.Balance
import Sepulcas.Elements

data MainPageNavbarItem
  = NavbarSend
  | NavbarHistory
  | NavbarReceive
  deriving (Eq, Show)

instance LocalizedPrint MainPageNavbarItem where
  localizedShow l v = case l of
    English -> case v of
      NavbarSend    -> "Send"
      NavbarHistory -> "History"
      NavbarReceive -> "Receive"
    Russian -> case v of
      NavbarSend    -> "Отправка"
      NavbarHistory -> "История"
      NavbarReceive -> "Получение"

mainPageNavbarItemToWidget :: MonadFront t m => MainPageNavbarItem -> m ()
mainPageNavbarItemToWidget NavbarSend = sendPageBtc Nothing
mainPageNavbarItemToWidget NavbarHistory = historyPage BTC
mainPageNavbarItemToWidget NavbarReceive = receivePage BTC

data NetworkPageNavbarItem
  = NavbarActiveIndexers
  | NavbarInactiveIndexers
  deriving (Eq, Show)

instance LocalizedPrint NetworkPageNavbarItem where
  localizedShow l v = case l of
    English -> case v of
      NavbarActiveIndexers   -> "Active indexers"
      NavbarInactiveIndexers -> "Inactive indexers"
    Russian -> case v of
      NavbarActiveIndexers   -> "Используемые индексаторы"
      NavbarInactiveIndexers -> "Неиспользуемые индексаторы"

networkPageNavbarItemToWidget :: MonadFront t m => NetworkPageNavbarItem -> m ()
networkPageNavbarItemToWidget NavbarActiveIndexers = activeIndexersPage
networkPageNavbarItemToWidget NavbarInactiveIndexers = inactiveIndexersPage

networkPageNavbarItemToWidgetUnauth :: MonadFrontBase t m => NetworkPageNavbarItem -> m ()
networkPageNavbarItemToWidgetUnauth NavbarActiveIndexers = activeIndexersPageUnauth
networkPageNavbarItemToWidgetUnauth NavbarInactiveIndexers = inactiveIndexersPageUnauth

navbarWidget :: MonadFront t m => Currency -> Maybe (Dynamic t (m ())) -> MainPageNavbarItem -> m ()
navbarWidget cur prevWidget activeItem = do
  when (activeItem /= NavbarReceive) localBalance
  divClass "container" $ divClass "navbar-3-cols" $ do
    sendE <- navbarBtn NavbarSend activeItem
    historyE <- navbarBtn NavbarHistory activeItem
    receiveE <- navbarBtn NavbarReceive activeItem
    switchNavbar prevWidget (mainPageNavbarItemToWidget <$> leftmost [sendE, historyE, receiveE])
  where
    localBalance = do
      balance <- balanceTitleWidget cur
      divClass "navbar-black py-1 px-2" $ do
        divClass "navbar-balance" $ dynText balance
        divClass "navbar-status"  $ statusBarWidget False cur

navbarBtn :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint a, Eq a) => a -> a -> m (Event t a)
navbarBtn item activeItem
  | item == activeItem = (item <$) <$> spanButton "navbar-item active" item
  | item /= activeItem = (item <$) <$> spanButton "navbar-item" item
navbarBtn _ _ = pure never

data HeaderNavigationButtonType = HeaderNavigationSend | HeaderNavigationReceive

navbarWidgetAndroid :: MonadFront t m => Currency -> Maybe (Dynamic t (m ())) -> m ()
navbarWidgetAndroid cur prevWidget = divClass "navbar-black py-1 px-2" $ do
    balance <- balanceTitleWidget cur
    divClass "navbar-balance" $ dynText balance
    divClass "navbar-status"  $ statusBarWidget False cur
    divClass "navbar-android-controls-wrapper" $ divClass "navbar-android-controls" $ do
      sendE <- divButton "navbar-android-controls-button mx-1" $ headerNavigationButton HeaderNavigationSend
      receiveE <- divButton "navbar-android-controls-button mx-1" $ headerNavigationButton HeaderNavigationReceive
      switchNavbar
        prevWidget
        ( leftmost
            [ mainPageNavbarItemToWidget NavbarSend <$ sendE,
              mainPageNavbarItemToWidget NavbarReceive <$ receiveE
            ]
        )
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

networkPageNavbarWidget :: MonadFront t m => Maybe (Dynamic t (m ())) -> NetworkPageNavbarItem -> m ()
networkPageNavbarWidget prevWidget activeItem = divClass "container" $ divClass "navbar-2-cols" $ mdo
  activeIndexersE <- navbarBtn NavbarActiveIndexers activeItem
  inactiveIndexersE <- navbarBtn NavbarInactiveIndexers activeItem
  switchNavbar prevWidget (networkPageNavbarItemToWidget <$> leftmost [activeIndexersE, inactiveIndexersE])

networkPageNavbarWidgetUnauth :: MonadFrontBase t m => Maybe (Dynamic t (m ())) -> NetworkPageNavbarItem -> m ()
networkPageNavbarWidgetUnauth prevWidget activeItem = divClass "container" $ divClass "navbar-2-cols" $ mdo
  activeIndexersE <- navbarBtn NavbarActiveIndexers activeItem
  inactiveIndexersE <- navbarBtn NavbarInactiveIndexers activeItem
  switchNavbar prevWidget (networkPageNavbarItemToWidgetUnauth <$> leftmost [activeIndexersE, inactiveIndexersE])

switchNavbar :: MonadFrontBase t m => Maybe (Dynamic t (m ())) -> Event t (m ()) -> m ()
switchNavbar prevWidget nextWidgetE = void $ nextWidget $ fforMaybe nextWidgetE $ \widget ->
  Just $ Retractable {
    retractableNext = widget
  , retractablePrev = prevWidget
  }
