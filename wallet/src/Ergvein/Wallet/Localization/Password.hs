module Ergvein.Wallet.Localization.Password
  (
    PasswordPageStrings(..)
  , PasswordWidgetStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

import Data.Text

data PasswordPageStrings = PPSTitle | PPSDescr | PPSUnlock

instance LocalizedPrint PasswordPageStrings where
  localizedShow l v = case l of
    English -> case v of
      PPSTitle  -> "Setup encryption password for your wallet"
      PPSDescr  -> "The password is used every time you perform an operation with your money. Leave the fields empty to set no password for your wallet."
      PPSUnlock -> "Unlock your private keys with password"
    Russian -> case v of
      PPSTitle  -> "Установите пароль шифрования для кошелька"
      PPSDescr  -> "Этот пароль используется для каждой операции с вашими деньгами. Можете оставить поле пустым, если хотите (не рекомендуется)"
      PPSUnlock -> "Введите пароль для расшифровки приватных ключей"

data PasswordWidgetStrings = PWSPassword | PWSRepeat | PWSSet | PWSNoMatch | PWSGo | PWSLogin | PWSEmptyLogin

instance LocalizedPrint PasswordWidgetStrings where
  localizedShow l v = case l of
    English -> case v of
      PWSPassword -> "Password"
      PWSRepeat   -> "Repeat password"
      PWSSet      -> "Set"
      PWSNoMatch  -> "Passwords do not match!"
      PWSGo       -> "Go"
      PWSLogin    -> "Login"
      PWSEmptyLogin -> "Login is empty!"
    Russian -> case v of
      PWSPassword -> "Пароль"
      PWSRepeat   -> "Повторите пароль"
      PWSSet      -> "Установить"
      PWSNoMatch  -> "Пароли не совпадают!"
      PWSGo       -> "Перейти"
      PWSLogin    -> "Логин"
      PWSEmptyLogin -> "Логин пустой!"
