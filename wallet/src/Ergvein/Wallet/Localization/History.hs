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
    HistoryBalance
  | HistoryNoTxs
  | HistoryUnconfirmed
  | HistoryUnconfirmedParents
  | HistoryTIBlockUndefined
  | HistoryTIAddressUndefined
  | HistoryTIFeeUndefined
  | HistoryTITitle
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

  deriving (Eq)

instance LocalizedPrint HistoryPageStrings where
  localizedShow l v = case l of
    English -> case v of
      HistoryBalance            -> "Balance"
      HistoryNoTxs              -> "No transactions yet"
      HistoryUnconfirmed        -> "Unconfirmed"
      HistoryUnconfirmedParents -> "Unconfirmed parents"
      HistoryTIBlockUndefined   -> "The transaction is not yet included in a block"
      HistoryTIAddressUndefined -> "Failed to decode address"
      HistoryTIFeeUndefined     -> "Unknown"
      HistoryTITitle            -> "Transaction info"
      HistoryTIVolume           -> "Volume"
      HistoryTIHash             -> "Hash"
      HistoryTILabel            -> "Label"
      HistoryTIURL              -> "Block Explorer"
      HistoryTIFee              -> "Fee"
      HistoryTIConfirmations    -> "Confirmations"
      HistoryTIBlock            -> "Block"
      HistoryTIRaw              -> "Raw"
      HistoryTIOutputs          -> "Outputs"
      HistoryTIInputs           -> "Inputs"
      HistoryTIOutputsAddress   -> "Address"
      HistoryTIOutputsValue     -> "Value"
      HistoryTIOutputsStatus    -> "Status"
    Russian -> case v of
      HistoryBalance            -> "Баланс"
      HistoryNoTxs              -> "Транзакций нет"
      HistoryUnconfirmed        -> "Не подтверждена"
      HistoryUnconfirmedParents -> "Родительские транзакции не подтверждены"
      HistoryTIBlockUndefined   -> "Транзакция еще не включена в блок"
      HistoryTIAddressUndefined -> "Не удалось декодировать адрес"
      HistoryTIFeeUndefined     -> "Неизвестно"
      HistoryTITitle            -> "Информация о транзакции"
      HistoryTIVolume           -> "Объем"
      HistoryTIHash             -> "Хэш"
      HistoryTILabel            -> "Описание"
      HistoryTIURL              -> "Block Explorer"
      HistoryTIFee              -> "Комиссия"
      HistoryTIConfirmations    -> "Подтверждения"
      HistoryTIBlock            -> "Блок"
      HistoryTIRaw              -> "Код"
      HistoryTIOutputs          -> "Выходы"
      HistoryTIInputs           -> "Входы"
      HistoryTIOutputsAddress   -> "Адрес"
      HistoryTIOutputsValue     -> "Объем"
      HistoryTIOutputsStatus    -> "Статус"
