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
  | STPSButActiveCurrs
  | STPSButUnits
  | STPSSelectLanguage
  | STPSSetsActiveCurrs
  | STPSSelectUnitsFor Currency
  | STPSSetsPinOn
  | STPSSetsPinOff
  | STPSSetsPinInput
  | STPSSetsPinDoSet

instance LocalizedPrint SettingsPageStrings where
  localizedShow l v = case l of
    English -> case v of
      STPSTitle               -> "Settings"
      STPSButLanguage         -> "Language"
      STPSButActiveCurrs      -> "Currencies"
      STPSButUnits            -> "Display units for cryptos"
      STPSSelectLanguage      -> "Select language:"
      STPSSetsActiveCurrs     -> "Settings for active currencies"
      STPSSelectUnitsFor cur  -> "Settings display units for " <> showt cur <> ":"
      STPSSetsPinOn           -> "Switch on PIN code"
      STPSSetsPinOff          -> "Switch off PIN code"
      STPSSetsPinInput        -> "Enter PIN code:"
      STPSSetsPinDoSet        -> "Set PIN code"
    Russian -> case v of
      STPSTitle               -> "Настройки"
      STPSButLanguage         -> "Язык"
      STPSButActiveCurrs      -> "Валюты"
      STPSButUnits            -> "Единицы отображения криптосистем"
      STPSSelectLanguage      -> "Выберите язык:"
      STPSSetsActiveCurrs     -> "Настройки активных валют"
      STPSSelectUnitsFor cur  -> "Выберите единиц отображения для " <> showt cur <> ":"
      STPSSetsPinOn           -> "Включить ПИН код"
      STPSSetsPinOff          -> "Выключить ПИН код"
      STPSSetsPinInput        -> "Введите ПИН код:"
      STPSSetsPinDoSet        -> "Установить ПИН код"

instance LocalizedPrint UnitBTC where
  localizedShow _ v = case v of
    BtcWhole    -> "btc"
    BtcMilli    -> "mbtc"
    BtcSat      -> "sat"

instance LocalizedPrint UnitERGO where
  localizedShow _ v = case v of
    ErgWhole -> "erg"
    ErgMilli -> "merg"
    ErgNano  -> "nerg"
