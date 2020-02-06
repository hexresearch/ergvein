module Ergvein.Wallet.Sync.Status(
    SyncBehind(..)
  , SyncStage(..)
  , SyncProgress(..)
  ) where 


import Data.Time 
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Reflex.Localize

data SyncBehind = SyncDays !Int | SyncHours !Int

instance LocalizedPrint SyncBehind where
  localizedShow l v = case l of
    English -> case v of
      SyncDays d  -> showt d <> if d == 1 then " day behind..." else " days behind..."
      SyncHours h -> showt h <> if h == 1 then " hour behind..." else " hours behind..."
    Russian -> case v of
      SyncDays d  -> "Отстаём на " <> showt d <> case (d `mod` 10) of
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

data SyncStage = SyncFilters | SyncHeaders | SyncBlocks
  deriving (Show)

instance LocalizedPrint SyncStage where 
  localizedShow l v = case l of 
    English -> case v of 
      SyncFilters -> "filters"
      SyncHeaders -> "headers"
      SyncBlocks  -> "blocks" 
    Russian -> case v of 
      SyncFilters -> "фильтров"
      SyncHeaders -> "заголовков"
      SyncBlocks  -> "блоков" 

data SyncProgress = 
    SyncMeta {
      syncMetaCur    :: !Currency
    , syncMetaStage  :: !SyncStage
    , syncMetaAmount :: !Int
    , syncMetaTotal  :: !Int
    }
  | Synced
  deriving (Show)

syncProgressBehind :: SyncProgress -> Maybe SyncBehind
syncProgressBehind v = case v of 
  SyncMeta{..} -> if syncMetaAmount >= syncMetaTotal then Nothing
    else Just $ nominalToBehind $ currencyBehind syncMetaCur syncMetaAmount syncMetaTotal
  Synced -> Nothing

instance LocalizedPrint SyncProgress where 
  localizedShow l v@SyncMeta{..} = case l of 
    English -> "Syncing " <> localizedShow l syncMetaStage <> " of " <> showt syncMetaCur <> precentStr
    Russian -> "Синхронизация " <> localizedShow l syncMetaStage <> " of " <> showt syncMetaCur <> precentStr
    where 
      percent = ceiling $ fromIntegral syncMetaAmount / fromIntegral syncMetaTotal
      precentStr = " " <> showt percent <> "% "
  localizedShow l Synced = case l of 
    English -> "Synced"
    Russian -> "Синхронизировано"
  