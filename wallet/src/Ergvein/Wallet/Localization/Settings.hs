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
  | STPSButUnits
  | STPSSelectLanguage
  | STPSSetsPinCode
  | STPSSelectUnitsBTC

instance LocalizedPrint SettingsPageStrings where
  localizedShow l v = case l of
    English -> case v of
      STPSTitle             -> "Settings"
      STPSButLanguage       -> "Language"
      STPSButPinCode        -> "PIN Code"
      STPSButUnits          -> "Display units for cryptos"
      STPSSelectLanguage    -> "Select language:"
      STPSSetsPinCode       -> "Settings for PIN code"
      STPSSelectUnitsBTC    -> "Settings display units for BTC:"
    Russian -> case v of
      STPSTitle             -> "Настройки"
      STPSButLanguage       -> "Язык"
      STPSButPinCode        -> "ПИН код"
      STPSButUnits          -> "Единицы отображения криптосистем"
      STPSSelectLanguage    -> "Выберите язык:"
      STPSSetsPinCode       -> "Настройки ПИН кода"
      STPSSelectUnitsBTC    -> "Выберите единиц отображения для BTC:"
