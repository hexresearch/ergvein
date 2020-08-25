module Ergvein.Wallet.Navbar.Types (
    NavbarItem (..)
  ) where

import Ergvein.Wallet.Language

data NavbarItem
  = NavbarSend
  | NavbarHistory
  | NavbarReceive
  deriving Eq

instance LocalizedPrint NavbarItem where
  localizedShow l v = case l of
    English -> case v of
      NavbarSend    -> "Send"
      NavbarHistory -> "History"
      NavbarReceive -> "Receive"
    Russian -> case v of
      NavbarSend    -> "Отправка"
      NavbarHistory -> "История"
      NavbarReceive -> "Получение"
