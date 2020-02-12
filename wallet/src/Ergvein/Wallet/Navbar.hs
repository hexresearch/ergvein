{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Navbar (
    navbarWidget
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Language

navbarWidget :: MonadFront t m => m ()
navbarWidget = divClass "navbar" $ do
  sendE <- spanBtn "navbar-item" NavbarSend
  historyE <- spanBtn "navbar-item active" NavbarHistory
  recieveE <- spanBtn "navbar-item" NavbarRecieve
  pure ()

data NavbarItem
  = NavbarSend
  | NavbarHistory
  | NavbarRecieve

instance LocalizedPrint NavbarItem where
  localizedShow l v = case l of
    English -> case v of
      NavbarSend    -> "Send"
      NavbarHistory -> "History"
      NavbarRecieve -> "Recieve"
    Russian -> case v of
      NavbarSend    -> "Отправка"
      NavbarHistory -> "Баланс"
      NavbarRecieve -> "Получение"

-- import Ergvein.Wallet.Elements
-- import Ergvein.Wallet.Embed
-- import Ergvein.Wallet.Embed.TH
-- import Ergvein.Wallet.Language
-- import {-# SOURCE #-} Ergvein.Wallet.Menu.Switcher
-- import Ergvein.Wallet.Menu.Types
-- import Ergvein.Wallet.Monad

-- menuWidget :: (MonadFront t m, LocalizedPrint a) => a -> Maybe (Dynamic t (m ())) -> m ()
-- menuWidget titleVal prevWidget = divClass "menu-header" $ do
--   divClass "menu-wallet-name" $ localizedText titleVal -- "Default wallet"
--   divClass "menu-wallet-menu" $ do
--     menuIconUrl <- createObjectURL menuIcon
--     divClass "menu-dropdown-wrapper" $ do
--       btnE <- divButton "menu-button" $ imgClass menuIconUrl ""
--       divClass "menu-dropdown" $ do
--         let menuBtn v = (v <$) <$> clearButton v
--         balE <- menuBtn MenuBalances
--         netE <- menuBtn MenuNetwork
--         setE <- menuBtn MenuSettings
--         abtE <- menuBtn MenuAbout
--         logE <- menuBtn MenuLogs
--         switchE <- menuBtn MenuSwitch
--         switchMenu prevWidget $ leftmost [balE, netE, setE, abtE, logE, switchE]