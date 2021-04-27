{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localize.Status
  (
    WalletStatusNormal(..)
  , RestoreStage(..)
  , CurrencyStatus(..)
  ) where

import Ergvein.Core.Status.Types
import Ergvein.Text
import Ergvein.Wallet.Language

instance LocalizedPrint WalletStatusNormal where
  localizedShow l v = case l of
    English -> case v of
      WalletStatusNormal'gettingNodeAddresses -> "Getting node addresses"
      WalletStatusNormal'connectingToPeers -> "Connecting to peers"
      WalletStatusNormal'gettingHeight h -> "Getting height. Catching up at: " <> showt h
      WalletStatusNormal'newFilters n -> showt n <> " new filters"
      WalletStatusNormal'synced -> "Fully synchronized"
      WalletStatusNormal'empty -> "Not active"
    Russian -> case v of
      WalletStatusNormal'gettingNodeAddresses -> "Получение адреса ноды"
      WalletStatusNormal'connectingToPeers -> "Подключение к узлу"
      WalletStatusNormal'gettingHeight h -> "Вычисление высоты. Сейчас на " <> showt h
      WalletStatusNormal'newFilters n -> showt n <> " новых фильтров"
      WalletStatusNormal'synced -> "Синхронизировано"
      WalletStatusNormal'empty -> "Отключена"

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
  localizedShow l (CurrencyStatus cur status) = case l of
    English -> "[" <> showt cur <> "]: " <> showt status
    Russian -> "[" <> showt cur <> "]: " <> showt status
