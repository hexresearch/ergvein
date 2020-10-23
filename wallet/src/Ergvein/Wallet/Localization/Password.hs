module Ergvein.Wallet.Localization.Password
  (
    PasswordPageStrings(..)
  , PasswordWidgetStrings(..)
  , LoginPageStrings(..)
  , PatternPageStrings(..)
  , ConfirmEmptyPage(..)
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

data PatternPageStrings = PatPSTitle | PatPSDescr | PatPSPass | PatPSPatt | PatPSUsePass | PatPSUsePattern

instance LocalizedPrint PatternPageStrings where
  localizedShow l v = case l of
    English -> case v of
      PatPSTitle      -> "Setup encryption pattern key for your wallet"
      PatPSDescr      -> "The pattern key is used every time you perform an operation with your money"
      PatPSPass       -> "Set password"
      PatPSPatt       -> "Set pattern"
      PatPSUsePass    -> "Use password"
      PatPSUsePattern -> "Use pattern"
    Russian -> case v of
      PatPSTitle      -> "Установите графический ключ шифрования для кошелька"
      PatPSDescr      -> "Этот графический ключ используется для каждой операции с вашими деньгами"
      PatPSPass       -> "Установить пароль"
      PatPSPatt       -> "Установить ключ"
      PatPSUsePass    -> "Ввести пароль"
      PatPSUsePattern -> "Ввести ключ"

data PasswordPageStrings = PPSTitle | PPSPassTitle | PPSDescr | PPSMnemonicTitle | PPSMnemonicDescr | PPSUnlock | PPSMnemonicUnlock | PPSWrongPassword
  deriving (Eq)

instance LocalizedPrint PasswordPageStrings where
  localizedShow l v = case l of
    English -> case v of
      PPSTitle          -> "Setup login and encryption password for your wallet"
      PPSPassTitle      -> "Setup encryption password for your wallet"
      PPSDescr          -> "The password is used every time you perform an operation with your money. Leave the password fields empty to set no password for your wallet (not recommended)."
      PPSMnemonicTitle  -> "Setup encryption password for your mnemonic phrase"
      PPSMnemonicDescr  -> "We ask you to set a separate password for compatibility between mobile and desktop versions of the application. Leave the fields empty to set no password for your mnemonic phrase (not recommended)."
      PPSUnlock         -> "Enter the password to unlock private storage"
      PPSMnemonicUnlock -> "Enter the password to decrypt the mnemonic phrase"
      PPSWrongPassword  -> "Wrong password"
    Russian -> case v of
      PPSTitle          -> "Установите логин и пароль для шифрования кошелька"
      PPSPassTitle      -> "Установите пароль для шифрования кошелька"
      PPSDescr          -> "Этот пароль используется для каждой операции с вашими деньгами. Можете оставить поля пароля пустыми, если хотите (не рекомендуется)."
      PPSMnemonicTitle  -> "Установите пароль для шифрования мнемонической фразы"
      PPSMnemonicDescr  -> "Мы просим Вас установить отдельный пароль для совместимости между мобильной и десктопной версией приложения. Можете оставить поля пустыми, если хотите (не рекомендуется)."
      PPSUnlock         -> "Введите пароль для расшифровки приватного хранилища"
      PPSMnemonicUnlock -> "Введите пароль для расшифровки мнемонической фразы"
      PPSWrongPassword  -> "Неверный пароль"

data PasswordWidgetStrings = PWSPassword | PWSPassNamed Text | PWSRepeat | PWSSet | PWSNoMatch | PWSGo | PWSLogin | PWSEmptyLogin | PWSEmptyPassword | PWSEmptyPattern | PWSDeriv | PWSDerivDescr | PWSInvalidPath

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
      PWSDeriv         -> "Derivation path"
      PWSDerivDescr    -> "You can override suggested derivation path. Leave it untouched if not sure what it is."
      PWSInvalidPath   -> "Derivation path is invalid! Format m/0'/0'/0'"
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
      PWSDeriv         -> "Префикс BIP48 для вывода ключей"
      PWSDerivDescr    -> "Вы можете переназначить префикс дерева для вывода ключей. Оставьте как есть, если не знаете, что это такое."
      PWSInvalidPath   -> "Путь вывода неверен! Формат поля m/0'/0'/0'"

data ConfirmEmptyPage = CEPBack | CEPSkip | CEPAttention | CEPConsequences

instance LocalizedPrint ConfirmEmptyPage where
  localizedShow l v = case l of
    English -> case v of
      CEPBack         -> "Back"
      CEPSkip         -> "Skip"
      CEPAttention    -> "The password is empty. Are you sure?"
      CEPConsequences -> "The wallet will be accesible without password"
    Russian -> case v of
      CEPBack         -> "Назад"
      CEPSkip         -> "Пропустить"
      CEPAttention    -> "Пустой пароль. Вы уверены?"
      CEPConsequences -> "Кошелёк будет доступен без ввода пароля"
