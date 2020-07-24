module Ergvein.Wallet.Menu.Types(
    MenuItem (..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad

data MenuItem
  = MenuBalances
  | MenuSingleBalance Currency
  | MenuNetwork
  | MenuSettings
  | MenuAbout
  | MenuLogs
  | MenuSwitch

instance LocalizedPrint MenuItem where
  localizedShow l v = case l of
    English -> case v of
      MenuBalances    -> "Balances"
      MenuSingleBalance cur -> showt cur <>  " balance"
      MenuNetwork     -> "Network"
      MenuSettings    -> "Settings"
      MenuAbout       -> "About"
      MenuLogs        -> "Logs"
      MenuSwitch      -> "Switch wallet"
    Russian -> case v of
      MenuBalances    -> "Балансы"
      MenuSingleBalance cur -> "Баланс" <> showt cur
      MenuNetwork     -> "Сеть"
      MenuSettings    -> "Настройки"
      MenuAbout       -> "О программе"
      MenuLogs        -> "Логи"
      MenuSwitch      -> "Сменить кошелёк"
