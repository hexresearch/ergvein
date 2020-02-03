module Ergvein.Wallet.Menu.Types(
    MenuItem (..)
  ) where

import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad

data MenuItem
  = MenuNetwork
  | MenuSettings
  | MenuAbout
  | MenuLogs
  | MenuSwitch

instance LocalizedPrint MenuItem where
  localizedShow l v = case l of
    English -> case v of
      MenuNetwork     -> "Network"
      MenuSettings    -> "Settings"
      MenuAbout       -> "About"
      MenuLogs        -> "Logs"
      MenuSwitch      -> "Switch wallet"
    Russian -> case v of
      MenuNetwork     -> "Сеть"
      MenuSettings    -> "Настройки"
      MenuAbout       -> "О программе"
      MenuLogs        -> "Логи"
      MenuSwitch      -> "Сменить кошелёк"
