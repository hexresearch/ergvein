module Ergvein.Wallet.Status.Types(
    SyncBehind(..)
  , StatusUpdate(..)
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

data StatusUpdate = StatGettingNodeAddresses
               | StatConnectingToPeers
               | StatGettingHeight !Int             -- Current height for catch up
               | StatNewFilters !Int
               | Synced
               | NotActive
  deriving (Show, Eq, Ord)

instance LocalizedPrint StatusUpdate where
  localizedShow l v = case l of
    English -> case v of
      StatGettingNodeAddresses -> "Getting node addresses"
      StatConnectingToPeers -> "Connecting to peers"
      StatGettingHeight h -> "Getting height. Catching up at: " <> showt h
      StatNewFilters n -> showt n <> " new filters"
      Synced -> "Fully synchronized"
      NotActive -> "Not active"
    Russian -> case v of
      StatGettingNodeAddresses -> "Получение адреса ноды"
      StatConnectingToPeers -> "Подключение к узлу"
      StatGettingHeight h -> "Вычисление высоты. Сейчас на " <> showt h
      StatNewFilters n -> showt n <> " новых фильтров"
      Synced -> "Синхронизировано"
      NotActive -> "Отключена"

data CurrencyStatus = CurrencyStatus !Currency !StatusUpdate
  deriving (Show, Eq)

instance LocalizedPrint CurrencyStatus where
  localizedShow l (CurrencyStatus cur stage) = case l of
    English -> "[" <> showt cur <> "]: " <> localizedShow l stage
    Russian -> "[" <> showt cur <> "]: " <> localizedShow l stage
