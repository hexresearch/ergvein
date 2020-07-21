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
  | HistoryTIAmount
  | HistoryTIHash
  | HistoryTITransactionId
  | HistoryTILabel
  | HistoryTIURL
  | HistoryTIFee
  | HistoryTIConfirmations
  | HistoryTIBlock
  | HistoryTIRaw
  | HistoryTIOutputs
  | HistoryTIInputs
  | HistoryTIOutputsAddress
  | HistoryTIOutputsOurAddress
  | HistoryTIOutputsValue
  | HistoryTIOutputsStatus

  deriving (Eq)

instance LocalizedPrint HistoryPageStrings where
  localizedShow l v = case l of
    English -> case v of
      HistoryBalance             -> "Balance"
      HistoryNoTxs               -> "No transactions yet"
      HistoryUnconfirmed         -> "Unconfirmed"
      HistoryUnconfirmedParents  -> "Unconfirmed parents"
      HistoryTIBlockUndefined    -> "The transaction is not yet included in a block"
      HistoryTIAddressUndefined  -> "Failed to decode address"
      HistoryTIFeeUndefined      -> "Unknown"
      HistoryTITitle             -> "Transaction info"
      HistoryTIAmount            -> "Amount"
      HistoryTIHash              -> "Hash"
      HistoryTITransactionId     -> "Transaction id"
      HistoryTILabel             -> "Label"
      HistoryTIURL               -> "Block Explorer"
      HistoryTIFee               -> "Fee"
      HistoryTIConfirmations     -> "Confirmations"
      HistoryTIBlock             -> "Block"
      HistoryTIRaw               -> "Raw"
      HistoryTIOutputs           -> "Outputs"
      HistoryTIInputs            -> "Inputs"
      HistoryTIOutputsAddress    -> "Address"
      HistoryTIOutputsOurAddress -> "Our address"
      HistoryTIOutputsValue      -> "Value"
      HistoryTIOutputsStatus     -> "Status"
    Russian -> case v of
      HistoryBalance             -> "Баланс"
      HistoryNoTxs               -> "Транзакций нет"
      HistoryUnconfirmed         -> "Не подтверждена"
      HistoryUnconfirmedParents  -> "Родительские транзакции не подтверждены"
      HistoryTIBlockUndefined    -> "Транзакция еще не включена в блок"
      HistoryTIAddressUndefined  -> "Не удалось декодировать адрес"
      HistoryTIFeeUndefined      -> "Неизвестно"
      HistoryTITitle             -> "Информация о транзакции"
      HistoryTIAmount            -> "Объем"
      HistoryTIHash              -> "Хэш"
      HistoryTITransactionId     -> "Id транзакции"
      HistoryTILabel             -> "Описание"
      HistoryTIURL               -> "Block Explorer"
      HistoryTIFee               -> "Комиссия"
      HistoryTIConfirmations     -> "Подтверждения"
      HistoryTIBlock             -> "Блок"
      HistoryTIRaw               -> "Код"
      HistoryTIOutputs           -> "Выходы"
      HistoryTIInputs            -> "Входы"
      HistoryTIOutputsAddress    -> "Адрес"
      HistoryTIOutputsOurAddress -> "Наш адрес"
      HistoryTIOutputsValue      -> "Объем"
      HistoryTIOutputsStatus     -> "Статус"
