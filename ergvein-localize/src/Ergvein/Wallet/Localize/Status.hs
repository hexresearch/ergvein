{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localize.Status
  (
    WalletStatusNormal(..)
  , RestoreStage(..)
  , CurrencyStatus(..)
  , ExchangeRatesError(..)
  ) where

import Ergvein.Core.Status.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language

instance LocalizedPrint WalletStatusNormal where
  localizedShow l v = case l of
    English -> case v of
      WalletStatusNormal'gettingNodeAddresses -> "Getting node addresses"
      WalletStatusNormal'connectingToPeers c -> "Connecting to " <> currencyName c <> " nodes"
      WalletStatusNormal'gettingHeight h -> "Getting height, current value: " <> showt h
      WalletStatusNormal'newFilters n -> "Scanning " <> showt n <> " new blocks"
      WalletStatusNormal'synced -> "Synced"
      WalletStatusNormal'empty -> ""
    Russian -> case v of
      WalletStatusNormal'gettingNodeAddresses -> "Получение адреса ноды"
      WalletStatusNormal'connectingToPeers c -> "Подключение к узлам " <> currencyName c
      WalletStatusNormal'gettingHeight h -> "Вычисление высоты, текущее значение: " <> showt h
      WalletStatusNormal'newFilters n -> "Сканируются " <> showt n <> " новых блоков"
      WalletStatusNormal'synced -> "Синхронизировано"
      WalletStatusNormal'empty -> ""

instance LocalizedPrint RestoreStage where
  localizedShow l v = case l of
    English -> case v of
      RestoreStage'connectingToBtcNodes -> "Connecting to Bitcoin nodes"
      RestoreStage'askingHeight         -> "Calculating the current height"
      RestoreStage'scanning             -> "Scanning blocks"
      RestoreStage'empty                -> "Loading..."
    Russian -> case v of
      RestoreStage'connectingToBtcNodes -> "Соединяемся с узлами Bitcoin"
      RestoreStage'askingHeight         -> "Вычисляем текущую высоту"
      RestoreStage'scanning             -> "Сканируем блоки"
      RestoreStage'empty                -> "Загрузка..."

instance LocalizedPrint CurrencyStatus where
  localizedShow l (CurrencyStatus cur status) =
      "[" <> showt cur <> "]: " <> localizedShow l status

data ExchangeRatesError = ExchangeRatesUnavailable

instance LocalizedPrint ExchangeRatesError where
  localizedShow l v = case l of
    English -> case v of
      ExchangeRatesUnavailable -> "Getting the exchange rate"
    Russian -> case v of
      ExchangeRatesUnavailable -> "Получение обменного курса"
