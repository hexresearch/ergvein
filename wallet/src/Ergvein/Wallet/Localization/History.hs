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
  | HistoryTIVolume
  | HistoryTIHash
  | HistoryTILabel
  | HistoryTIURL
  | HistoryTIFee
  | HistoryTIConfirmations
  | HistoryTIBlock
  | HistoryTIRaw
  | HistoryTIOutputs
  | HistoryTIInputs
  | HistoryTIOutputsAddress
  | HistoryTIOutputsValue
  | HistoryTIOutputsStatus
  | HistoryTIBalance
  deriving (Eq)

instance LocalizedPrint HistoryPageStrings where
  localizedShow l v = case l of
    English -> case v of
      HistoryTITitle          -> "Transaction info"
      HistoryTIVolume         -> "Volume"
      HistoryTIHash           -> "Hash"
      HistoryTILabel          -> "Label"
      HistoryTIURL            -> "Block Explorer"
      HistoryTIFee            -> "Fee"
      HistoryTIConfirmations  -> "Confirmations"
      HistoryTIBlock          -> "Block"
      HistoryTIRaw            -> "Raw"
      HistoryTIOutputs        -> "Outputs"
      HistoryTIInputs         -> "Inputs"
      HistoryTIOutputsAddress -> "Address"
      HistoryTIOutputsValue   -> "Value"
      HistoryTIOutputsStatus  -> "Status"
      HistoryTIBalance        -> "Balance"
    Russian -> case v of
      HistoryTITitle          -> "Информация о транзакции"
      HistoryTIVolume         -> "Объем"
      HistoryTIHash           -> "Хэш"
      HistoryTILabel          -> "Описание"
      HistoryTIURL            -> "Block Explorer"
      HistoryTIFee            -> "Комиссия"
      HistoryTIConfirmations  -> "Подтверждения"
      HistoryTIBlock          -> "Блок"
      HistoryTIRaw            -> "Код"
      HistoryTIOutputs        -> "Выходы"
      HistoryTIInputs         -> "Входы"
      HistoryTIOutputsAddress -> "Адрес"
      HistoryTIOutputsValue   -> "Объем"
      HistoryTIOutputsStatus  -> "Статус"
      HistoryTIBalance        -> "Баланс"
