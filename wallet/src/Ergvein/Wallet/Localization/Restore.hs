module Ergvein.Wallet.Localization.Restore
  (
    RestorePageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Transaction
import Ergvein.Wallet.Language
import Numeric
import Data.Text (pack)

data RestorePageStrings
  = RPSProgress Double
  | RPSConnecting
  | RPSGetHeight
  | RPSGetFiltsTitle
  | RPSGetFiltsFromTo BlockHeight BlockHeight
  | RPSScanTitle
  | RPSBlocksTitle
  | RPSBlocskAmount Int
  | RPSBlockDelayWarn
  | RPSKeysTitle
  | RPSScanProgress BlockHeight
  | RPSFinished
  | RPSTrafficTitle
  | RPSTrafficWarn
  | RPSTrafficWifi
  | RPSTrafficTime
  | RPSTrafficAccept
  | RPSInProgress

instance LocalizedPrint RestorePageStrings where
  localizedShow l v = case l of
    English -> case v of
      RPSConnecting         -> "Connecting to Bitcoin nodes"
      RPSGetHeight          -> "Calculating the current height"
      RPSGetFiltsTitle      -> "Getting the next batch of filters"
      RPSGetFiltsFromTo f t -> "From " <> showt f <> " to " <> showt t
      RPSScanTitle          -> "Concurrently scanning the batch"
      RPSBlocksTitle        -> "Getting blocks"
      RPSBlocskAmount n     -> "Hit " <> showt n <> " blocks"
      RPSBlockDelayWarn     -> "This might take a while"
      RPSKeysTitle          -> "Updating wallet keys"
      RPSScanProgress h     -> "Scanning height " <> showt h
      RPSFinished           -> "Restore completed"
      RPSTrafficTitle       -> "Beware of traffic!"
      RPSTrafficWarn        -> "The restore will download about 400-500Mb"
      RPSTrafficWifi        -> "You are advised to use wi-fi or a connection with unlimited traffic"
      RPSTrafficTime        -> "The whole process can take about 30 minutes"
      RPSTrafficAccept      -> "Continue"
      RPSInProgress         -> "Restoration progress"
    Russian -> case v of
      RPSConnecting         -> "Соединяемся с узлами Bitcoin"
      RPSGetHeight          -> "Вычисляем текущую высоту"
      RPSGetFiltsTitle      -> "Запрашиваем следующую пачку фильтров"
      RPSGetFiltsFromTo f t -> "От " <> showt f <> " до " <> showt t
      RPSScanTitle          -> "Параллельно сканируем фильтры"
      RPSBlocksTitle        -> "Получаем блоки"
      RPSBlocskAmount n     -> "Проверяем " <> showt n <> " блоков"
      RPSBlockDelayWarn     -> "Запрос блоков может затянуться"
      RPSKeysTitle          -> "Обновляем ключи кошелька"
      RPSScanProgress h     -> "Сейчас сканируется высота: " <> showt h
      RPSFinished           -> "Восстановление успешно завершено"
      RPSTrafficTitle       -> "Внимание! Траффик!"
      RPSTrafficWarn        -> "Восстановление скачает примерно 400-500Мб"
      RPSTrafficWifi        -> "Рекомендуем соединиться через wi-fi или безлимитные сети"
      RPSTrafficTime        -> "Процесс может занять около 30 минут"
      RPSTrafficAccept      -> "Продолжить"
      RPSInProgress         -> "Прогресс восстановления"
