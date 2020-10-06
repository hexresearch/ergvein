module Ergvein.Wallet.Sync.Status(
    SyncBehind(..)
  , SyncStage(..)
  , SyncProgress(..)
  , syncProgressBehind
  ) where

import Data.Time
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Platform

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

data SyncStage = SyncFilters !Int !Int              -- amount total
               | SyncAddressInternal !Int !Int !Int -- addr #, amount, total
               | SyncAddressExternal !Int !Int !Int -- addr #, amount, total
               | SyncBlocks !Int !Int !Int !Int     -- something something amount, total
               | SyncGettingNodeAddresses
               | SyncConnectingToPeers
               | SyncGettingHeight !Int             -- Current height for catch up
               | SyncConnectionIndexer
               | SyncNoIndexer
               | Synced
               | NotActive
  deriving (Show, Eq, Ord)

instance LocalizedPrint SyncStage where
  localizedShow l v = case l of
    English -> case v of
      SyncFilters a t -> "filters " <> showt (percent a t) <> "%"
      SyncAddressInternal i a t -> "#" <> showt i <> " " <> showt (percent a t) <> "%"
      SyncAddressExternal i a t -> "#" <> showt i <> " " <> showt (percent a t) <> "%"
      SyncBlocks i j a t -> showt i <> " of " <> showt j <> " " <> showt (percent a t) <> "%"
      SyncGettingNodeAddresses -> "Getting node addresses"
      SyncConnectingToPeers -> "Connecting to peers"
      SyncGettingHeight h -> "Getting height. Catching up at: " <> showt h
      SyncConnectionIndexer -> "Connecting to indexer"
      SyncNoIndexer -> "All indexers are down"
      Synced -> "Fully synchronized"
      NotActive -> "Not active"
    Russian -> case v of
      SyncFilters a t -> "фильтров " <> showt (percent a t) <> "%"
      SyncAddressInternal i a t -> "#" <> showt i <> " " <> showt (percent a t) <> "%"
      SyncAddressExternal i a t -> "#" <> showt i <> " " <> showt (percent a t) <> "%"
      SyncBlocks i j a t -> showt i <> " из " <> showt j <> " " <> showt (percent a t) <> "%"
      SyncGettingNodeAddresses -> "Получение адреса ноды"
      SyncConnectingToPeers -> "Подключение к узлу"
      SyncGettingHeight h -> "Вычисление высоты. Сейчас на " <> showt h
      SyncConnectionIndexer -> "Подключение к индексатору"
      SyncNoIndexer -> "Все индексаторы недоступны"
      Synced -> "Синхронизировано"
      NotActive -> "Отключена"

data SyncProgress = SyncProgress !Currency !SyncStage
  deriving (Show, Eq)

getAmountTotal :: SyncStage -> Maybe (Int, Int)
getAmountTotal v = case v of
  SyncFilters a t -> Just (a,t)
  SyncAddressInternal _ a t -> Just (a,t)
  SyncAddressExternal _ a t -> Just (a,t)
  SyncBlocks _ _ a t -> Just (a,t)
  _ -> Nothing

syncProgressBehind :: SyncProgress -> Maybe SyncBehind
syncProgressBehind (SyncProgress cur v) = case getAmountTotal v of
  Just (amount, total) -> if amount >= total then Nothing
    else Just $ nominalToBehind $ currencyBehind cur amount total
  _ -> Nothing

instance LocalizedPrint SyncProgress where
  localizedShow l (SyncProgress cur stage) = case l of
    English -> "Sync [" <> showt cur <> "]: " <> localizedShow l stage
    Russian -> "Синх. [" <> showt cur <> "]: " <> localizedShow l stage

percent :: Int -> Int -> Int
percent amount total = if total == 0 then 0 else ceiling $ 100 * (fromIntegral amount :: Double) / fromIntegral total
