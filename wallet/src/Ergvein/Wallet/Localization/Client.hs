module Ergvein.Wallet.Localization.Client
  (
    ClientMessage(..)
  , ClientErr(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

data ClientMessage
  = CMSLoading Int Int Int
  | CMSError
  | CMSEmpty
  | CMSValidationError
  | CMSDone
  | CMSTimeout
  | CMSRestarting
  | CMSAllOutOfSync
  deriving (Eq)

instance LocalizedPrint ClientMessage where
  localizedShow l v = case l of
    English -> case v of
      CMSLoading i mi ma  -> "Loading: " <> showt i <> " of " <> showt ma <> " (min: " <> showt mi <> ")"
      CMSError            -> "A request has failed"
      CMSEmpty            -> "Results are empty"
      CMSValidationError  -> "Validation error: inconsistent results"
      CMSDone             -> "Done!"
      CMSTimeout          -> "Time out!"
      CMSRestarting       -> "Restarting"
      CMSAllOutOfSync     -> "All indexers are out of sync!"
    Russian -> case v of
      CMSLoading i mi ma  -> "Запрашиваю. " <> showt i <> " из " <> showt ma <> " (мин: " <> showt mi <> ") ответили."
      CMSError            -> "Один из запросов не удался"
      CMSEmpty            -> "Результатов нет"
      CMSValidationError  -> "Ошибка: противоречивые ответы"
      CMSDone             -> "Готово!"
      CMSTimeout          -> "Время вышло!"
      CMSRestarting       -> "Перезапускаем"
      CMSAllOutOfSync     -> "Все индексаторы отстают от цепочки!"

data ClientErr = ClientErrInconsistentResult | ClientErrNoUrls | ClientErrTimeOut
  deriving (Eq, Show)

instance LocalizedPrint ClientErr where
  localizedShow l v = case l of
    English -> case v of
      ClientErrInconsistentResult -> "Failed to fetch. Inconsistent results"
      ClientErrNoUrls             -> "No more nodes to query"
      ClientErrTimeOut            -> "All requests timed out"
    Russian -> case v of
      ClientErrInconsistentResult -> "Запрос провален: приходят противоречивые ответы"
      ClientErrNoUrls             -> "Больше некому слать запрос"
      ClientErrTimeOut            -> "Все запросы зависли"
