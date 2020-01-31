module Ergvein.Wallet.Localization.Settings(
    SettingsPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language

import Data.Text

data SettingsPageStrings =
    STPSTitle
  | STPSButLanguage
  | STPSButPinCode
  | STPSButUnits
  | STPSSelectLanguage
  | STPSSetsPinCode
  | STPSSelectUnitsFor Currency

instance LocalizedPrint SettingsPageStrings where
  localizedShow l v = case l of
    English -> case v of
      STPSTitle               -> "Settings"
      STPSButLanguage         -> "Language"
      STPSButPinCode          -> "PIN Code"
      STPSButUnits            -> "Display units for cryptos"
      STPSSelectLanguage      -> "Select language:"
      STPSSetsPinCode         -> "Settings for PIN code"
      STPSSelectUnitsFor cur  -> "Settings display units for " <> showt cur <> ":"
    Russian -> case v of
      STPSTitle               -> "Настройки"
      STPSButLanguage         -> "Язык"
      STPSButPinCode          -> "ПИН код"
      STPSButUnits            -> "Единицы отображения криптосистем"
      STPSSelectLanguage      -> "Выберите язык:"
      STPSSetsPinCode         -> "Настройки ПИН кода"
      STPSSelectUnitsFor cur  -> "Выберите единиц отображения для " <> showt cur <> ":"

instance LocalizedPrint UnitBTC where
  localizedShow _ v = case v of
    BTC_BTC     -> "BTC"
    BTC_mBTC    -> "mBTC"
    BTC_uBTC    -> "uBTC"
    BTC_satoshi -> "satoshi"

instance LocalizedPrint UnitERGO where
  localizedShow _ v = case v of
    ERGO_ERGO -> "ERGO"
