{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Status.Types(
    WalletStatus(..)
  , WalletStatusNormal(..)
  , WalletStatusRestore(..)
  , emptyWalletStatus
    -- SyncBehind(..)
  , CurrencyStatus(..)
  , nominalToBehind
  ) where

import Data.Time
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language

data SyncBehind = SyncDays !Int | SyncHours !Int

instance LocalizedPrint SyncBehind where
  localizedShow l v = case l of
    English -> case v of
      SyncDays d | d < 365 -> showt d <> if d == 1 then " day behind..." else " days behind..."
      SyncDays d -> showt (d `div` 365) <> " years " <> showt (d `mod` 365) <> if d == 1 then " day behind..." else " days behind..."
      SyncHours h -> showt h <> if h == 1 then " hour behind..." else " hours behind..."
    Russian -> case v of
      SyncDays d | d < 365 -> "Отстаём на " <> showt d <> case (d `mod` 10) of
        1 -> " день..."
        2 -> " дня..."
        3 -> " дня..."
        _ -> " дней..."
      SyncDays d  -> "Отстаём на " <> showt (d `div` 365) <> yearsEnding <> showt (d `mod` 365) <> daysEnding
        where
          yearsEnding = case d `div` 365 of
            1 -> " год "
            2 -> " года "
            3 -> " года "
            4 -> " года "
            _ -> " лет "
          daysEnding = case (d `mod` 10) of
            1 -> " день..."
            2 -> " дня..."
            3 -> " дня..."
            _ -> " дней..."
      SyncHours h -> "Отстаём на " <> showt h <> case (h `mod` 10) of
        1 -> " час..."
        2 -> " часа..."
        3 -> " часа..."
        _ -> " часов..."

nominalToBehind :: NominalDiffTime -> SyncBehind
nominalToBehind t
  | t < 24 * 3600 = SyncHours $ ceiling $ t / 3600
  | otherwise = SyncDays $ ceiling $ t / (24 * 3600)

data WalletStatus = WalletStatus {
    walletStatus'normal :: WalletStatusNormal
  , walletStatus'restore :: WalletStatusRestore
} deriving (Show, Eq)

data WalletStatusNormal =
    WalletStatusNormal'gettingNodeAddresses
  | WalletStatusNormal'connectingToPeers
  | WalletStatusNormal'gettingHeight !Int             -- Current height for catch up
  | WalletStatusNormal'newFilters !Int
  | WalletStatusNormal'synced
  | WalletStatusNormal'empty
  deriving (Show, Eq, Ord)

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

data CurrencyStatus = CurrencyStatus !Currency !WalletStatusNormal
  deriving (Show, Eq)

instance LocalizedPrint CurrencyStatus where
  localizedShow l (CurrencyStatus cur status) = case l of
    English -> "[" <> showt cur <> "]: " <> showt status
    Russian -> "[" <> showt cur <> "]: " <> showt status

data RestoreStage =
    RestoreStage'connectingToBtcNodes
  | RestoreStage'askingHeight
  | RestoreStage'scanning
  | RestoreStage'restoreFinished
  | RestoreStage'empty
  deriving (Show, Eq)

data WalletStatusRestore = WalletStatusRestore {
    walletStatusRestore'stage :: RestoreStage
  , walletStatusRestore'progress :: Maybe Double
} deriving (Show, Eq)

emptyRestoreStatus :: WalletStatusRestore
emptyRestoreStatus = WalletStatusRestore {
      walletStatusRestore'stage = RestoreStage'empty
    , walletStatusRestore'progress = Nothing
  }

emptyWalletStatus :: WalletStatus
emptyWalletStatus = WalletStatus {
      walletStatus'normal = WalletStatusNormal'empty
    , walletStatus'restore = emptyRestoreStatus
  }
