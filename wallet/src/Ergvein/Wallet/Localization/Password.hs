module Ergvein.Wallet.Localization.Password
  (
    PasswordPageStrings(..)
  , PasswordWidgetStrings(..)
  , LoginPageStrings(..)
  , PatternPageStrings(..)
  ) where

import Ergvein.Wallet.Language

data LoginPageStrings = LPSTitle | LPSDescr

instance LocalizedPrint LoginPageStrings where
  localizedShow l v = case l of
    English -> case v of
      LPSTitle  -> "Set login name for wallet"
      LPSDescr -> "You could have several wallets and name helps to separate them."
    Russian -> case v of
      LPSTitle  -> "Установите логин для кошелька"
      LPSDescr -> "Вы можете иметь несколько кошельков и имя поможет различать их"

data PatternPageStrings = PatPSTitle | PatPSDescr

instance LocalizedPrint PatternPageStrings where
  localizedShow l v = case l of
    English -> case v of
      PatPSTitle -> "Setup encryption pattern key for your wallet"
      PatPSDescr -> "The pattern key is used every time you perform an operation with your money"
    Russian -> case v of
      PatPSTitle -> "Установите графический ключ шифрования для кошелька"
      PatPSDescr -> "Этот графический ключ используется для каждой операции с вашими деньгами"

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

data PasswordWidgetStrings = PWSPassword | PWSRepeat | PWSSet | PWSNoMatch | PWSGo | PWSLogin | PWSEmptyLogin | PWSEmptyPassword | PWSEmptyPattern

instance LocalizedPrint PasswordWidgetStrings where
  localizedShow l v = case l of
    English -> case v of
      PWSPassword      -> "Password"
      PWSRepeat        -> "Repeat password"
      PWSSet           -> "Set"
      PWSNoMatch       -> "Passwords do not match!"
      PWSGo            -> "Go"
      PWSLogin         -> "Login"
      PWSEmptyLogin    -> "Login is empty!"
      PWSEmptyPassword -> "Password is empty!"
      PWSEmptyPattern  -> ""
    Russian -> case v of
      PWSPassword      -> "Пароль"
      PWSRepeat        -> "Повторите пароль"
      PWSSet           -> "Установить"
      PWSNoMatch       -> "Пароли не совпадают!"
      PWSGo            -> "Перейти"
      PWSLogin         -> "Логин"
      PWSEmptyLogin    -> "Логин пустой!"
      PWSEmptyPassword -> "Пароль пустой!"
      PWSEmptyPattern  -> ""
