module Ergvein.Wallet.Localize.Receive
  (
    ReceiveTitle(..)
   ,ReceivePageStrings(..)
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Language

newtype ReceiveTitle = ReceiveTitle Currency

instance LocalizedPrint ReceiveTitle where
  localizedShow l (ReceiveTitle c) = case l of
    English -> "Receive " <> currencyName c
    Russian -> "Получить " <> currencyName c

data ReceivePageStrings =
    RPSTitle
  | RPSAddLabel
  | RPSGap
  | RPSGenNew
  | RPSNewLabel
instance LocalizedPrint ReceivePageStrings where
  localizedShow l v = case l of
    English -> case v of
      RPSAddLabel -> "Add label"
      RPSGap      -> "Exceeded gap limit"
      RPSGenNew   -> "Generate new"
      RPSNewLabel -> "New label"
      RPSTitle    -> "Receive"
    Russian -> case v of
      RPSAddLabel -> "Добавить название"
      RPSGap      -> "Вышли за \"Gap limit\""
      RPSGenNew   -> "Создать новый"
      RPSNewLabel -> "Изменить название"
      RPSTitle    -> "Получение"
