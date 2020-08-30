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
import Reflex.Localize

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

data SyncStage = SyncFilters | SyncAddressInternal !Int | SyncAddressExternal !Int | SyncBlocks !Int !Int
  deriving (Show, Eq, Ord)

instance LocalizedPrint SyncStage where
  localizedShow l v = case l of
    English -> case v of
      SyncFilters -> "filters"
      SyncAddressInternal i -> "#" <> showt i
      SyncAddressExternal i -> "#" <> showt i
      SyncBlocks i j -> showt i <> " of " <> showt j
    Russian -> case v of
      SyncFilters -> "фильтров"
      SyncAddressInternal i -> "#" <> showt i
      SyncAddressExternal i -> "#" <> showt i
      SyncBlocks i j -> showt i <> " из " <> showt j

data SyncProgress =
    SyncMeta {
      syncMetaCur    :: !Currency
    , syncMetaStage  :: !SyncStage
    , syncMetaAmount :: !Int
    , syncMetaTotal  :: !Int
    }
  | Synced
  deriving (Show, Eq)

syncProgressBehind :: SyncProgress -> Maybe SyncBehind
syncProgressBehind v = case v of
  SyncMeta{..} -> if syncMetaAmount >= syncMetaTotal then Nothing
    else Just $ nominalToBehind $ currencyBehind syncMetaCur syncMetaAmount syncMetaTotal
  Synced -> Nothing

instance LocalizedPrint SyncProgress where
  localizedShow l SyncMeta{syncMetaStage = syncMetaStage@SyncFilters, ..} = case l of
    English -> "Syncing " <> localizedShow l syncMetaStage <> " of " <> showt syncMetaCur <> ": " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."
    Russian -> "Синхронизация " <> localizedShow l syncMetaStage <> " " <> showt syncMetaCur <> ": " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."

  localizedShow l SyncMeta{syncMetaStage = syncMetaStage@(SyncAddressInternal _), ..} = case l of
    English -> "Syncing " <> showt syncMetaCur <> " change address " <> localizedShow l syncMetaStage <> ", " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."
    Russian -> "Синхронизация " <> showt syncMetaCur <> " адреса для сдачи " <> localizedShow l syncMetaStage <> ", " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."

  localizedShow l SyncMeta{syncMetaStage = syncMetaStage@(SyncAddressExternal _), ..} = case l of
    English -> "Syncing " <> showt syncMetaCur <> " receiving address " <> localizedShow l syncMetaStage <> ", " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."
    Russian -> "Синхронизация " <> showt syncMetaCur <> " адреса для получения " <> localizedShow l syncMetaStage <> ", " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."

  localizedShow l SyncMeta{syncMetaStage = syncMetaStage@(SyncBlocks _ _), ..} = if isAndroid
    then case l of
      English -> "Syncing new blocks of " <> showt syncMetaCur <> ": " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."
      Russian -> "Синхронизация новых блоков " <> showt syncMetaCur <> ": " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."
     else case l of
      English -> "Syncing new blocks of " <> showt syncMetaCur <> ": " <> localizedShow l syncMetaStage <> ", " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."
      Russian -> "Синхронизация новых блоков " <> showt syncMetaCur <> ": " <> localizedShow l syncMetaStage <> ", " <> showt (percent syncMetaAmount syncMetaTotal) <> "%."

  localizedShow l Synced = case l of
    English -> "Fully synced"
    Russian -> "Полностью синхронизировано"

percent :: Int -> Int -> Int
percent amount total = if total == 0 then 0 else ceiling $ 100 * (fromIntegral amount :: Double) / fromIntegral total
