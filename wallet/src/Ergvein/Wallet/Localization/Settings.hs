module Ergvein.Wallet.Localization.Settings(
    SettingsPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

import Data.Text

data SettingsPageStrings =
    STPSTitle
  | STPSButLanguage
  | STPSButPinCode
  | STPSSelectLanguage
  | STPSSetsPinCode
  | STPSSetsPinOn
  | STPSSetsPinOff
  | STPSSetsPinInput
  | STPSSetsPinDoSet

instance LocalizedPrint SettingsPageStrings where
  localizedShow l v = case l of
    English -> case v of
      STPSTitle             -> "Settings"
      STPSButLanguage       -> "Language"
      STPSButPinCode        -> "PIN Code"
      STPSSelectLanguage    -> "Select language:"
      STPSSetsPinCode       -> "Settings for PIN code"
      STPSSetsPinOn         -> "Switch on PIN code"
      STPSSetsPinOff        -> "Switch off PIN code"
      STPSSetsPinInput      -> "Enter PIN code:"
      STPSSetsPinDoSet      -> "Set PIN code"
    Russian -> case v of
      STPSTitle             -> "Настройки"
      STPSButLanguage       -> "Язык"
      STPSButPinCode        -> "ПИН код"
      STPSSelectLanguage    -> "Выберите язык:"
      STPSSetsPinCode       -> "Настройки ПИН кода"
      STPSSetsPinOn         -> "Включить ПИН код"
      STPSSetsPinOff        -> "Выключить ПИН код"
      STPSSetsPinInput      -> "Введите ПИН код:"
      STPSSetsPinDoSet      -> "Установить ПИН код"
