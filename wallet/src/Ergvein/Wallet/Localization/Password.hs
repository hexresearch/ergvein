module Ergvein.Wallet.Localization.Password
  (
    PasswordPageStrings(..)
  , PasswordWidgetStrings(..)
  , LoginPageStrings(..)
  , PatternPageStrings(..)
  ) where

import Data.Text (Text)

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

data PasswordPageStrings = PPSTitle | PPSDescr | PPSMnemonicTitle | PPSMnemonicDescr | PPSUnlock | PPSMnemonicUnlock | PPSWrongPassword
  deriving (Eq)

instance LocalizedPrint PasswordPageStrings where
  localizedShow l v = case l of
    English -> case v of
      PPSTitle          -> "Setup login and encryption password for your wallet"
      PPSDescr          -> "The password is used every time you perform an operation with your money. Leave the password fields empty to set no password for your wallet (not recommended)."
      PPSMnemonicTitle  -> "Setup encryption password for your mnemonic phrase"
      PPSMnemonicDescr  -> "We ask you to set a separate password for compatibility between mobile and desktop versions of the application. Leave the fields empty to set no password for your mnemonic phrase (not recommended)."
      PPSUnlock         -> "Enter the password to unlock private storage"
      PPSMnemonicUnlock -> "Enter the password to decrypt the mnemonic phrase"
      PPSWrongPassword  -> "Wrong password"
    Russian -> case v of
      PPSTitle          -> "Установите логин и пароль для шифрования кошелька"
      PPSDescr          -> "Этот пароль используется для каждой операции с вашими деньгами. Можете оставить поля пароля пустыми, если хотите (не рекомендуется)."
      PPSMnemonicTitle  -> "Установите пароль для шифрования мнемонической фразы"
      PPSMnemonicDescr  -> "Мы просим Вас установить отдельный пароль для совместимости между мобильной и десктопной версией приложения. Можете оставить поля пустыми, если хотите (не рекомендуется)."
      PPSUnlock         -> "Введите пароль для расшифровки приватного хранилища"
      PPSMnemonicUnlock -> "Введите пароль для расшифровки мнемонической фразы"
      PPSWrongPassword  -> "Неверный пароль"

data PasswordWidgetStrings = PWSPassword | PWSPassNamed Text | PWSRepeat | PWSSet | PWSNoMatch | PWSGo | PWSLogin | PWSEmptyLogin | PWSEmptyPassword | PWSEmptyPattern

instance LocalizedPrint PasswordWidgetStrings where
  localizedShow l v = case l of
    English -> case v of
      PWSPassword      -> "Password"
      PWSPassNamed n   -> "Password for " <> n
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
      PWSPassNamed n   -> "Пароль от " <> n
      PWSRepeat        -> "Повторите пароль"
      PWSSet           -> "Установить"
      PWSNoMatch       -> "Пароли не совпадают!"
      PWSGo            -> "Далее"
      PWSLogin         -> "Логин"
      PWSEmptyLogin    -> "Логин пустой!"
      PWSEmptyPassword -> "Пароль пустой!"
      PWSEmptyPattern  -> ""
