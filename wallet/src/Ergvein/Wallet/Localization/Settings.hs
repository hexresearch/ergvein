{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localization.Settings(
    SettingsPageStrings(..)
  , NetSetupStrings(..)
  ) where

import Data.Time
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Wallet.Language
import Ergvein.Wallet.Elements.Input.Class
import Ergvein.Wallet.Localization.Input
import Ergvein.Wallet.Localization.IP

data SettingsPageStrings =
    STPSTitle
  | STPSButLanguage
  | STPSButActiveCurrs
  | STPSButNetwork
  | STPSButUnits
  | STPSButPortfolio
  | STPSButMnemonicExport
  | STPSButDns
  | STPSButTor
  | STPSButNodes
  | STPSButSetPass
  | STPSSelectLanguage
  | STPSSetsTor
  | STPSUseTor
  | STPSSetsProxy
  | STPSProxyIpField
  | STPSProxyPortField
  | STPSSetsActiveCurrs
  | STPSSetsPortfolio
  | STPSSetsPortfolioEnable
  | STPSSetsFiatSelect
  | STPSSelectUnitsFor Currency
  | STPSSetsPinOn
  | STPSSetsPinOff
  | STPSSetsPinInput
  | STPSSetsPinDoSet
  | STPSMnemonicExportMsg
  | STPSSuccess
  | STPSIPStrings IPStrings
  | STPSInputStrings InputStrings
  deriving (Eq, Show)

instance Wrappable IPStrings SettingsPageStrings where
  wrap = STPSIPStrings

instance Wrappable InputStrings SettingsPageStrings where
  wrap = STPSInputStrings

instance LocalizedPrint SettingsPageStrings where
  localizedShow l v = case l of
    English -> case v of
      STPSTitle               -> "Settings"
      STPSButLanguage         -> "Language"
      STPSButActiveCurrs      -> "Currencies"
      STPSButNetwork          -> "Network"
      STPSButDns              -> "DNS servers"
      STPSButTor              -> "Tor and proxy"
      STPSButNodes            -> "BTC nodes"
      STPSButSetPass          -> "Change password"
      STPSButUnits            -> "Display units for cryptos"
      STPSButPortfolio        -> "Portfolio"
      STPSButMnemonicExport   -> "Export mnemonic phrase"
      STPSSelectLanguage      -> "Select language:"
      STPSSetsTor             -> "Tor configuration"
      STPSUseTor              -> "Use Tor"
      STPSSetsProxy           -> "SOCKS proxy configuration"
      STPSProxyIpField        -> "Proxy IP address"
      STPSProxyPortField      -> "Proxy port"
      STPSSetsActiveCurrs     -> "Settings for active currencies"
      STPSSetsPortfolio       -> "Settings for fiat and ticks"
      STPSSetsPortfolioEnable -> "Display portfolio"
      STPSSetsFiatSelect      -> "Select fiat"
      STPSSelectUnitsFor cur  -> "Settings display units for " <> showt cur <> ":"
      STPSSetsPinOn           -> "Switch on PIN code"
      STPSSetsPinOff          -> "Switch off PIN code"
      STPSSetsPinInput        -> "Enter PIN code:"
      STPSSetsPinDoSet        -> "Set PIN code"
      STPSMnemonicExportMsg   -> "This is your password protected mnemonic phrase in QR code and text form. Choose the most convenient way."
      STPSSuccess             -> "Successfully updated settings"
      STPSIPStrings s         -> localizedShow English s
      STPSInputStrings s      -> localizedShow English s
    Russian -> case v of
      STPSTitle               -> "Настройки"
      STPSButLanguage         -> "Язык"
      STPSButActiveCurrs      -> "Валюты"
      STPSButNetwork          -> "Сеть"
      STPSButDns              -> "DNS сервера"
      STPSButTor              -> "Tor и прокси"
      STPSButNodes            -> "Ноды BTC"
      STPSButSetPass          -> "Изменить пароль"
      STPSButUnits            -> "Единицы отображения криптосистем"
      STPSButPortfolio        -> "Портфель"
      STPSButMnemonicExport   -> "Экспортировать мнемоническую фразу"
      STPSSelectLanguage      -> "Выберите язык:"
      STPSSetsTor             -> "Настройки Tor"
      STPSUseTor              -> "Проксировать через Tor"
      STPSSetsProxy           -> "Настройки прокси SOCKS"
      STPSProxyIpField        -> "Адрес прокси"
      STPSProxyPortField      -> "Порт прокси"
      STPSSetsActiveCurrs     -> "Настройки активных валют"
      STPSSetsPortfolio       -> "Настройки отображения фиата и тиков"
      STPSSetsPortfolioEnable -> "Отображение портфеля"
      STPSSetsFiatSelect      -> "Выберите фиат"
      STPSSelectUnitsFor cur  -> "Выберите единиц отображения для " <> showt cur <> ":"
      STPSSetsPinOn           -> "Включить ПИН код"
      STPSSetsPinOff          -> "Выключить ПИН код"
      STPSSetsPinInput        -> "Введите ПИН код:"
      STPSSetsPinDoSet        -> "Установить ПИН код"
      STPSMnemonicExportMsg   -> "Это ваша защищенная паролем мнемоническая фраза в виде QR-кода и в текстовом виде. Используйте наиболее подходящий для вас способ."
      STPSSuccess             -> "Настройки успешно обновлены"
      STPSIPStrings s         -> localizedShow Russian s
      STPSInputStrings s      -> localizedShow Russian s

instance LocalizedPrint UnitBTC where
  localizedShow _ v = case v of
    BtcWhole    -> "BTC"
    BtcMilli    -> "mBTC"
    BtcSat      -> "sat"

instance LocalizedPrint UnitERGO where
  localizedShow _ v = case v of
    ErgWhole -> "ERG"
    ErgMilli -> "mERG"
    ErgNano  -> "nERG"

data NetSetupStrings
  = NSSTitle
  | NSSLatency NominalDiffTime
  | NSSIndexerHeight BlockHeight
  | NSSOffline
  | NSSRefresh
  | NSSPing
  | NSSPingAll
  | NSSDisable
  | NSSEnable
  | NSSForget
  | NSSResolveConfDefault
  | NSSRestoreUrls
  | NSSRestoreDef
  | NSSReqTimeout
  | NSSActUrlNum
  | NSSReqNumMin
  | NSSReqNumMax
  | NSSSave
  | NSSAddUrl
  | NSSAddDns
  | NSSAdd
  | NSSDelete
  | NSSEdit
  | NSSCancel
  | NSSClose
  | NSSCopyURL
  | NSSFailedDns
  | NSSNoHeight

instance LocalizedPrint NetSetupStrings where
  localizedShow l v = case l of
    English -> case v of
      NSSTitle        -> "Network settings"
      NSSLatency lat  -> "Latency: " <> showt lat
      NSSIndexerHeight h -> "Height: " <> showt h
      NSSOffline      -> "Offline"
      NSSPing         -> "Ping"
      NSSDisable      -> "Disable"
      NSSEnable       -> "Enable"
      NSSForget       -> "Forget"
      NSSPingAll      -> "Ping all"
      NSSResolveConfDefault -> "Using servers from system configuration"
      NSSRestoreUrls  -> "Restore default"
      NSSRestoreDef   -> "Restore default values"
      NSSReqTimeout   -> "Request timeout, s"
      NSSActUrlNum    -> "Min. number of active indexers"
      NSSReqNumMin    -> "Min. number of confirmations"
      NSSReqNumMax    -> "Required number of confirmations"
      NSSSave         -> "Save"
      NSSAddUrl       -> "Add indexer"
      NSSAddDns       -> "Add DNS"
      NSSAdd          -> "Add"
      NSSDelete       -> "Delete"
      NSSEdit         -> "Edit"
      NSSCancel       -> "Cancel"
      NSSClose        -> "Close"
      NSSCopyURL      -> "Copy URL"
      NSSFailedDns    -> "Failed to parse DNS IP"
      NSSNoHeight     -> "None"
    Russian -> case v of
      NSSTitle        -> "Настройки сети"
      NSSLatency lat  -> "Задержка: " <> showt lat
      NSSIndexerHeight h -> "Высота: " <> showt h
      NSSOffline      -> "Оффлайн"
      NSSPing         -> "Запросить статус"
      NSSDisable      -> "Отключить"
      NSSEnable       -> "Включить"
      NSSForget       -> "Забыть"
      NSSPingAll      -> "Запросить всех"
      NSSResolveConfDefault -> "Используем глобальные настройки системы"
      NSSRestoreUrls  -> "Сервера по умолчанию"
      NSSRestoreDef   -> "Значения по умолчанию"
      NSSReqTimeout   -> "Время ожидания ответа, с"
      NSSActUrlNum    -> "Минимальное количество активных серверов"
      NSSReqNumMin    -> "Минимальное количество подтверждений"
      NSSReqNumMax    -> "Необходимое количество подтверждений"
      NSSSave         -> "Сохранить"
      NSSAddUrl       -> "Добавить индексер"
      NSSAddDns       -> "Добавить DNS"
      NSSAdd          -> "Добавить"
      NSSDelete       -> "Удалить"
      NSSEdit         -> "Изменить"
      NSSCancel       -> "Отменить"
      NSSClose        -> "Закрыть"
      NSSCopyURL      -> "Copy URL"
      NSSFailedDns    -> "Некорректный IP DNS сервера"
      NSSNoHeight     -> "Нет"
