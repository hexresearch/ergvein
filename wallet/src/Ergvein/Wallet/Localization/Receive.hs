module Ergvein.Wallet.Localization.Receive
  (
    ReceiveTitle(..)
   ,ReceivePageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language

import Data.Text

newtype ReceiveTitle = ReceiveTitle Currency

instance LocalizedPrint ReceiveTitle where
  localizedShow l (ReceiveTitle c) = case l of
    English -> "Receive " <> currencyName c
    Russian -> "Получить " <> currencyName c


data ReceivePageStrings =
    RPSTitle
  | RPSAddLabel
  | RPSNewLabel
  | RPSGenNew
  | RPSCopy
  | RPSGap

instance LocalizedPrint ReceivePageStrings where
  localizedShow l v = case l of
    English -> case v of
      RPSTitle    -> "Receive"
      RPSAddLabel -> "Add label"
      RPSNewLabel -> "New label"
      RPSGenNew   -> "Generate new"
      RPSCopy     -> "Copy"
      RPSGap      -> "Exceeded gap limit"
    Russian -> case v of
      RPSTitle    -> "Получение"
      RPSAddLabel -> "Добавить название"
      RPSNewLabel -> "Изменить название"
      RPSGenNew   -> "Создать новый"
      RPSCopy     -> "Скопировать"
      RPSGap      -> "Вышли за \"Gap limit\""
