module Ergvein.Wallet.Menu(
    menuWidget
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Embed
import Ergvein.Wallet.Embed.TH
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad

data MenuItems = MenuNetwork | MenuSettings | MenuAbout | MenuLogs

instance LocalizedPrint MenuItems where
  localizedShow l v = case l of
    English -> case v of
      MenuNetwork -> "Network"
      MenuSettings -> "Settings"
      MenuAbout -> "About"
      MenuLogs -> "Logs"
    Russian -> case v of
      MenuNetwork -> "Сеть"
      MenuSettings -> "Настройки"
      MenuAbout -> "О программе"
      MenuLogs -> "Логи"

menuWidget :: MonadFrontBase t m => m ()
menuWidget = do
  divClass "menu-header" $ do
    divClass "menu-wallet-name" $ text "Default wallet"
    divClass "menu-wallet-menu" $ do
      menuIconUrl <- createObjectURL menuIcon
      divClass "menu-dropdown-wrapper" $ do
        btnE <- divButton "menu-button" $ imgClass menuIconUrl ""
        divClass "menu-dropdown" $ do
          clearButton MenuNetwork
          clearButton MenuSettings
          clearButton MenuAbout
          clearButton MenuLogs
          pure ()
      pure ()
