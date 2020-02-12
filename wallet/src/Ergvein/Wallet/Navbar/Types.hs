module Ergvein.Wallet.Navbar.Types (
    NavbarItem (..)
  ) where


import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad

data NavbarItem
  = NavbarSend
  | NavbarHistory
  | NavbarRecieve
  deriving Eq

instance LocalizedPrint NavbarItem where
  localizedShow l v = case l of
    English -> case v of
      NavbarSend    -> "Send"
      NavbarHistory -> "History"
      NavbarRecieve -> "Recieve"
    Russian -> case v of
      NavbarSend    -> "Отправка"
      NavbarHistory -> "История"
      NavbarRecieve -> "Получение"