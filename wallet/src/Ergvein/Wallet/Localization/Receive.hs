module Ergvein.Wallet.Localization.Receive
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
  | RPSCopy
  | RPSGap
  | RPSGenNew
  | RPSNewLabel
  | RPSShare
  | RPSShareQR

instance LocalizedPrint ReceivePageStrings where
  localizedShow l v = case l of
    English -> case v of
      RPSAddLabel -> "Add label"
      RPSCopy     -> "Copy"
      RPSGap      -> "Exceeded gap limit"
      RPSGenNew   -> "Generate new"
      RPSNewLabel -> "New label"
      RPSShare    -> "Share"
      RPSShareQR  -> "Share QR-code"
      RPSTitle    -> "Receive"
    Russian -> case v of
      RPSAddLabel -> "Добавить название"
      RPSCopy     -> "Скопировать"
      RPSGap      -> "Вышли за \"Gap limit\""
      RPSGenNew   -> "Создать новый"
      RPSNewLabel -> "Изменить название"
      RPSShare    -> "Поделиться"
      RPSShareQR  -> "Поделиться QR-кодом"
      RPSTitle    -> "Получение"
