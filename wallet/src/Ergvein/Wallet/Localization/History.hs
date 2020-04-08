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
      HistoryTIURL            -> "Blockexplorer URL"
      HistoryTIFee            -> "Fee"
      HistoryTIConfirmations  -> "Confirmations"
      HistoryTIBlock          -> "Block"
      HistoryTIRaw            -> "Raw"
      HistoryTIOutputs        -> "Outputs"
      HistoryTIInputs         -> "Inputs"
    Russian -> case v of
      HistoryTITitle          -> "Слова мнемоники от вашего кошелька"
      HistoryTIHash           -> "Хэш"
      HistoryTIURL            -> "Blockexplorer URL"
      HistoryTIFee            -> "Комиссия"
      HistoryTIConfirmations  -> "Подтверждения"
      HistoryTIBlock          -> "Блок"
      HistoryTIRaw            -> "Код"
      HistoryTIOutputs        -> "Выходы"
      HistoryTIInputs         -> "Входы"
