module Ergvein.Wallet.Localize.Password
  (
    PasswordPageStrings(..)
  , PasswordWidgetStrings(..)
  , LoginPageStrings(..)
  , PinCodePageStrings(..)
  , ConfirmEmptyPage(..)
  , StartHeightStrings(..)
  , PasswordTypePageStrings(..)
  , ChangePasswordStrings(..)
  ) where

import Data.Time
import Data.Text (Text)

import Ergvein.Types.Transaction
import Ergvein.Wallet.Language

import qualified Data.Text as T
import Ergvein.Types (Password)

data LoginPageStrings = LPSTitle | LPSDescr

instance LocalizedPrint LoginPageStrings where
  localizedShow l v = case l of
    English -> case v of
      LPSTitle  -> "Set login name for wallet"
      LPSDescr -> "You could have several wallets and name helps to separate them."
    Russian -> case v of
      LPSTitle  -> "Установите логин для кошелька"
      LPSDescr -> "Вы можете иметь несколько кошельков и имя поможет различать их"

data PasswordTypePageStrings = PasswordTypeTitle | PasswordTypeText | PasswordTypePin | PasswordTypeEmpty

instance LocalizedPrint PasswordTypePageStrings where
  localizedShow l v = case l of
    English -> case v of
      PasswordTypeTitle -> "Select password type"
      PasswordTypeText  -> "Text password"
      PasswordTypePin   -> "PIN"
      PasswordTypeEmpty -> "No password"
    Russian -> case v of
      PasswordTypeTitle -> "Выберите тип пароля"
      PasswordTypeText  -> "Текстовый пароль"
      PasswordTypePin   -> "ПИН-код"
      PasswordTypeEmpty -> "Без пароля"

data PinCodePageStrings =
    PinCodePSTitle
  | PinCodePSPinCodeLengthRange
  | PinCodePSTooShortError
  | PinCodePSConfirm
  | PinCodePSConfirmationError
  | PinCodePSEnterPinCode

instance LocalizedPrint PinCodePageStrings where
  localizedShow l v = case l of
    English -> case v of
      PinCodePSTitle -> "Set security PIN"
      PinCodePSPinCodeLengthRange -> "PIN must be 6 to 12 digits long"
      PinCodePSTooShortError -> "PIN code must be at least 6 digits"
      PinCodePSConfirm -> "Repeat security PIN"
      PinCodePSConfirmationError -> "PINs do not match"
      PinCodePSEnterPinCode -> "Enter your secret PIN"
    Russian -> case v of
      PinCodePSTitle -> "Задайте ПИН-код"
      PinCodePSPinCodeLengthRange -> "ПИН-код должен содержать от 6 до 12 цифр"
      PinCodePSTooShortError -> "ПИН-код должен содержать не менее 6 цифр"
      PinCodePSConfirm -> "Повторите ПИН-код"
      PinCodePSConfirmationError -> "ПИН-коды не совпадают"
      PinCodePSEnterPinCode -> "Введите ПИН-код"

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

data PasswordWidgetStrings =
    PWSPassword
  | PWSPassNamed Text
  | PWSRepeat
  | PWSSet
  | PWSNoMatch
  | PWSGo
  | PWSLogin
  | PWSEmptyLogin
  | PWSEmptyPassword
  | PWSDeriv
  | PWSDerivDescr1
  | PWSDerivDescr2
  | PWSInvalidPath
  | PWSMoreOptions
  | PWSLessOptions

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
      PWSDeriv         -> "Derivation path"
      PWSDerivDescr1   -> "You can override the suggested derivation path."
      PWSDerivDescr2   -> "Leave it untouched if not sure what it is."
      PWSInvalidPath   -> "Derivation path is invalid! Format m/0'/0'/0'"
      PWSMoreOptions   -> "Advanced settings"
      PWSLessOptions   -> "Advanced settings"
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
      PWSDeriv         -> "Префикс BIP48 для вывода ключей"
      PWSDerivDescr1   -> "Вы можете переназначить префикс дерева для вывода ключей."
      PWSDerivDescr2   -> "Оставьте как есть, если не знаете, что это такое."
      PWSInvalidPath   -> "Путь вывода неверен! Формат поля m/0'/0'/0'"
      PWSMoreOptions   -> "Дополнительные настройки"
      PWSLessOptions   -> "Дополнительные настройки"

data ConfirmEmptyPage = CEPBack | CEPSkip | CEPAttention | CEPConsequences | CEPSure

instance LocalizedPrint ConfirmEmptyPage where
  localizedShow l v = case l of
    English -> case v of
      CEPBack         -> "Back"
      CEPSkip         -> "Skip"
      CEPAttention    -> "The password is empty. Are you sure?"
      CEPConsequences -> "The wallet will be accesible without password"
      CEPSure         -> "I am sure"
    Russian -> case v of
      CEPBack         -> "Назад"
      CEPSkip         -> "Пропустить"
      CEPAttention    -> "Пустой пароль. Вы уверены?"
      CEPConsequences -> "Кошелёк будет доступен без ввода пароля"
      CEPSure         -> "Я уверен"

data StartHeightStrings = SHSDescr | SHSLabel | SHSParseError | SHSNonNegError | SHSEstimate BlockHeight

instance LocalizedPrint StartHeightStrings where
  localizedShow l v = case l of
    English -> case v of
      SHSDescr        -> "The height to restore from"
      SHSLabel        -> "Starting height"
      SHSParseError   -> "Parse error. Not an integer!"
      SHSNonNegError  -> "The height has to be non-negative!"
      SHSEstimate h   -> "Estimated start date: " <> showDate h
    Russian -> case v of
      SHSDescr        -> "Начальная высота для восстановления"
      SHSLabel        -> "Высота"
      SHSParseError   -> "Ошибка. Введите целое число"
      SHSNonNegError  -> "Высота должна быть 0 или больше"
      SHSEstimate h   -> "Дата начала (оценка): " <> showDate h

data ChangePasswordStrings = CPSTitle | CPSDescr | CPSOld

instance LocalizedPrint ChangePasswordStrings where
  localizedShow l v = case l of
    English -> case v of
      CPSTitle -> "Change password"
      CPSDescr -> "Enter the new password"
      CPSOld   -> "You will have to enter the old password at the end"
    Russian -> case v of
      CPSTitle -> "Смена пароля"
      CPSDescr -> "Введите новый пароль"
      CPSOld   -> "В конце вам понадобится ввести старый пароль"

showDate :: BlockHeight -> Text
showDate h = T.pack $ formatTime defaultTimeLocale "%F" $ addUTCTime delta genDate
  where
    genDate = parseTimeOrError False defaultTimeLocale "%F %T" "2009-01-03 18:15:05"
    delta = fromIntegral h * 9.5 * 60
