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
  | STPSButPortfolio
  | STPSSelectLanguage
  | STPSSetsActiveCurrs
  | STPSSetsPortfolio
  | STPSSetsPortfolioEnable
  | STPSSetsFiatSelect
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
      STPSButPortfolio        -> "Portfolio"
      STPSSelectLanguage      -> "Select language:"
      STPSSetsActiveCurrs     -> "Settings for active currencies"
      STPSSetsPortfolio       -> "Settings for fiat and ticks"
      STPSSetsPortfolioEnable -> "Display portfolio"
      STPSSetsFiatSelect      -> "Select fiat"
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
      STPSButPortfolio        -> "Портфель"
      STPSSelectLanguage      -> "Выберите язык:"
      STPSSetsActiveCurrs     -> "Настройки активных валют"
      STPSSetsPortfolio       -> "Настройки отображения фиата и тиков"
      STPSSetsPortfolioEnable -> "Отображение портфеля"
      STPSSetsFiatSelect      -> "Выберите фиат"
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
  | NSSEdit
  | NSSClose
  | NSSCopyURL

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
      NSSEdit         -> "Edit"
      NSSClose        -> "Close"
      NSSCopyURL      -> "Copy URL"
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
      NSSRestoreUrls  -> "Сервера по умолчанию"
      NSSRestoreDef   -> "Значения по умолчанию"
      NSSReqTimeout   -> "Время ожидания ответа, с"
      NSSActUrlNum    -> "Минимальное количество активных серверов"
      NSSReqNumMin    -> "Минимальное количество подтверждений"
      NSSReqNumMax    -> "Необходимое количество подтверждений"
      NSSSave         -> "Сохранить"
      NSSAddUrl       -> "Добавить индексер"
      NSSAdd          -> "Добавить"
      NSSEdit         -> "Изменить"
      NSSClose        -> "Закрыть"
      NSSCopyURL      -> "Copy URL"
