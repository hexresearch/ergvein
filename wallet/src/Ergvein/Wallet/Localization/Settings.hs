module Ergvein.Wallet.Localization.Settings(
    SettingsPageStrings(..)
  , NetSetupStrings(..)
  ) where

import Data.Text
import Data.Time
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language

data SettingsPageStrings =
    STPSTitle
  | STPSButLanguage
  | STPSButActiveCurrs
  | STPSButNetwork
  | STPSButUnits
  | STPSSelectLanguage
  | STPSSetsActiveCurrs
  | STPSSelectUnitsFor Currency
  | STPSSetsPinOn
  | STPSSetsPinOff
  | STPSSetsPinInput
  | STPSSetsPinDoSet
  | STPSSuccess
  deriving (Eq)

instance LocalizedPrint SettingsPageStrings where
  localizedShow l v = case l of
    English -> case v of
      STPSTitle               -> "Settings"
      STPSButLanguage         -> "Language"
      STPSButActiveCurrs      -> "Currencies"
      STPSButNetwork          -> "Network"
      STPSButUnits            -> "Display units for cryptos"
      STPSSelectLanguage      -> "Select language:"
      STPSSetsActiveCurrs     -> "Settings for active currencies"
      STPSSelectUnitsFor cur  -> "Settings display units for " <> showt cur <> ":"
      STPSSetsPinOn           -> "Switch on PIN code"
      STPSSetsPinOff          -> "Switch off PIN code"
      STPSSetsPinInput        -> "Enter PIN code:"
      STPSSetsPinDoSet        -> "Set PIN code"
      STPSSuccess             -> "Successfully updated settings"
    Russian -> case v of
      STPSTitle               -> "Настройки"
      STPSButLanguage         -> "Язык"
      STPSButActiveCurrs      -> "Валюты"
      STPSButNetwork          -> "Сеть"
      STPSButUnits            -> "Единицы отображения криптосистем"
      STPSSelectLanguage      -> "Выберите язык:"
      STPSSetsActiveCurrs     -> "Настройки активных валют"
      STPSSelectUnitsFor cur  -> "Выберите единиц отображения для " <> showt cur <> ":"
      STPSSetsPinOn           -> "Включить ПИН код"
      STPSSetsPinOff          -> "Выключить ПИН код"
      STPSSetsPinInput        -> "Введите ПИН код:"
      STPSSetsPinDoSet        -> "Установить ПИН код"
      STPSSuccess             -> "Настройки успешно обновлены"

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

data NetSetupStrings
  = NSSTitle
  | NSSLatency NominalDiffTime
  | NSSOffline
  | NSSRefresh
  | NSSPing
  | NSSPingAll
  | NSSDisable
  | NSSEnable
  | NSSForget
  | NSSRestoreUrls
  | NSSRestoreDef
  | NSSReqTimeout
  | NSSActUrlNum
  | NSSReqNumMin
  | NSSReqNumMax
  | NSSSave
  | NSSAddUrl
  | NSSAdd
  | NSSClose

instance LocalizedPrint NetSetupStrings where
  localizedShow l v = case l of
    English -> case v of
      NSSTitle        -> "Network settings"
      NSSLatency lat  -> "Latency: " <> showt lat
      NSSOffline      -> "Offline"
      NSSRefresh      -> "Refresh"
      NSSPing         -> "Ping"
      NSSDisable      -> "Disable"
      NSSEnable       -> "Enable"
      NSSForget       -> "Forget"
      NSSPingAll      -> "Ping all"
      NSSRestoreUrls  -> "Add default URLs"
      NSSRestoreDef   -> "Restore default values"
      NSSReqTimeout   -> "Request timeout, s"
      NSSActUrlNum    -> "Min. number of active indexers"
      NSSReqNumMin    -> "Min. number of confirmations"
      NSSReqNumMax    -> "Required number of confirmations"
      NSSSave         -> "Save"
      NSSAddUrl       -> "Add indexer"
      NSSAdd          -> "Add"
      NSSClose        -> "Close"
    Russian -> case v of
      NSSTitle        -> "Настройки сети"
      NSSLatency lat  -> "Задержка: " <> showt lat
      NSSOffline      -> "Оффлайн"
      NSSRefresh      -> "Обновить"
      NSSPing         -> "Запросить статус"
      NSSDisable      -> "Отключить"
      NSSEnable       -> "Включить"
      NSSForget       -> "Забыть"
      NSSPingAll      -> "Запросить всех"
      NSSRestoreUrls  -> "Восстановить сервера по умолчанию"
      NSSRestoreDef   -> "Восстановить значения по умолчанию"
      NSSReqTimeout   -> "Время ожидания ответа, с"
      NSSActUrlNum    -> "Минимальное количество активных серверов"
      NSSReqNumMin    -> "Минимальное количество подтверждений"
      NSSReqNumMax    -> "Необходимое количество подтверждений"
      NSSSave         -> "Сохранить"
      NSSAddUrl       -> "Добавить индексер"
      NSSAdd          -> "Добавить"
      NSSClose        -> "Закрыть"
