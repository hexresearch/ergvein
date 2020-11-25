module Ergvein.Wallet.Localization.Restore
  (
    RestorePageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Transaction
import Ergvein.Wallet.Language
import Numeric
import Reflex.Localize
import Data.Text (pack)

data RestorePageStrings
  = RPSProgress Double
  | RPSConnecting
  | RPSGetHeight
  | RPSGetFiltsTitle
  | RPSGetFiltsFromTo BlockHeight BlockHeight
  | RPSScanTitle
  | RPSScanProgress BlockHeight
  | RPSFinished

instance LocalizedPrint RestorePageStrings where
  localizedShow l v = case l of
    English -> case v of
      RPSProgress p         -> "Restore progress: " <> (pack $ showFFloat (Just 2) p "") <> "% done"
      RPSConnecting         -> "Connecting to BTC nodes"
      RPSGetHeight          -> "Calculating the current height"
      RPSGetFiltsTitle      -> "Getting the next batch of filters"
      RPSGetFiltsFromTo f t -> "From " <> showt f <> " to " <> showt t
      RPSScanTitle          -> "Concurrently scanning the batch ..."
      RPSScanProgress h     -> "Scanning height " <> showt h
      RPSFinished           -> "Restore completed"
    Russian -> case v of
      RPSProgress p         -> "Восстановление: " <> (pack $ showFFloat (Just 2) p "") <> "% завершено"
      RPSConnecting         -> "Соединяемся с узлами BTC"
      RPSGetHeight          -> "Вычисляем текущую высоту"
      RPSGetFiltsTitle      -> "Запрашиваем следующую пачку фильтров"
      RPSGetFiltsFromTo f t -> "От " <> showt f <> " до " <> showt t
      RPSScanTitle          -> "Параллельно сканируем фильтры"
      RPSScanProgress h     -> "Сейчас сканируется высота: " <> showt h
      RPSFinished           -> "Восстановление успешно завершено"
