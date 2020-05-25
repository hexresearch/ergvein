module Ergvein.Wallet.Localization.Receive
  (
    RecieveTitle(..)
   ,ReceivePageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language

import Data.Text

newtype RecieveTitle = RecieveTitle Currency

instance LocalizedPrint RecieveTitle where
  localizedShow l (RecieveTitle c) = case l of
    English -> "Receive " <> currencyName c
    Russian -> "Получить " <> currencyName c


data ReceivePageStrings =
    RPSTitle
  | RPSAddLabel



instance LocalizedPrint ReceivePageStrings where
  localizedShow l v = case l of
    English -> case v of
      RPSTitle        -> "Receive"
      RPSAddLabel         -> "Add label"
    Russian -> case v of
      RPSTitle        -> "Получение"
      RPSAddLabel         -> "Добавить название"
