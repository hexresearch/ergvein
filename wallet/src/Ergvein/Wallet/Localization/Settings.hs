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

instance LocalizedPrint SettingsPageStrings where
  localizedShow l v = case l of
    English -> case v of
      STPSTitle             -> "Settings"
      STPSButLanguage       -> "Language"
      STPSButPinCode        -> "PIN Code"
      STPSSelectLanguage    -> "Select language:"
      STPSSetsPinCode       -> "Settings for PIN code"
    Russian -> case v of
      STPSTitle             -> "Настройки"
      STPSButLanguage       -> "Язык"
      STPSButPinCode        -> "ПИН код"
      STPSSelectLanguage    -> "Выберите язык:"
      STPSSetsPinCode       -> "Настройки ПИН кода"
