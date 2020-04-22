module Ergvein.Wallet.Localization.History
  (
    HistoryPageStrings(..)
  , HistoryTitle(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language

import Data.Text

newtype HistoryTitle = HistoryTitle Currency

instance LocalizedPrint HistoryTitle where
  localizedShow l (HistoryTitle c) = case l of
    English -> "History " <> currencyName c
    Russian -> "История " <> currencyName c

data HistoryPageStrings =
    HistoryTITitle
  | HistoryTIHash
  | HistoryTILabel
  | HistoryTIURL
  | HistoryTIFee
  | HistoryTIConfirmations
  | HistoryTIBlock
  | HistoryTIRaw
  | HistoryTIOutputs
  | HistoryTIInputs

instance LocalizedPrint HistoryPageStrings where
  localizedShow l v = case l of
    English -> case v of
      HistoryTITitle          -> "Transaction info"
      HistoryTIHash           -> "Hash"
      HistoryTILabel          -> "Label"
      HistoryTIURL            -> "Blockexplorer"
      HistoryTIFee            -> "Fee"
      HistoryTIConfirmations  -> "Confirmations"
      HistoryTIBlock          -> "Block"
      HistoryTIRaw            -> "Raw"
      HistoryTIOutputs        -> "Outputs"
      HistoryTIInputs         -> "Inputs"
    Russian -> case v of
      HistoryTITitle          -> "Слова мнемоники от вашего кошелька"
      HistoryTIHash           -> "Хэш"
      HistoryTILabel          -> "Название"
      HistoryTIURL            -> "Blockexplorer"
      HistoryTIFee            -> "Комиссия"
      HistoryTIConfirmations  -> "Подтверждения"
      HistoryTIBlock          -> "Блок"
      HistoryTIRaw            -> "Код"
      HistoryTIOutputs        -> "Выходы"
      HistoryTIInputs         -> "Входы"
