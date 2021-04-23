module Ergvein.Wallet.Localize.Balance(
    BalancesStrings
  ) where

import Ergvein.Wallet.Language

data BalancesStrings
  = BalancesTitle
  | ButtonSend
  | ButtonReceive

instance LocalizedPrint BalancesStrings where
  localizedShow l v = case l of
    English -> case v of
      BalancesTitle -> "Default wallet"
      ButtonSend    -> "Send"
      ButtonReceive -> "Receive"
    Russian -> case v of
      BalancesTitle -> "Стандартный кошелек"
      ButtonSend    -> "Отправить"
      ButtonReceive -> "Получить"
