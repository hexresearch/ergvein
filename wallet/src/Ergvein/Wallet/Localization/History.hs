module Ergvein.Wallet.Localization.History
  (
    HistoryPageStrings(..)
  , HistoryTitle(..)
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Language

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
  | HistorySend
  | HistoryReceive
  | HistoryTIBlockUndefined
  | HistoryTIAddressUndefined
  | HistoryTIFeeUndefined
  | HistoryTITitle
  | HistoryTIAmount
  | HistoryTIWalletChanges
  | HistoryTIHash
  | HistoryTITransactionId
  | HistoryTILabel
  | HistoryTIURL
  | HistoryTIFee
  | HistoryTIRbf
  | HistoryTIConflictingTxs
  | HistoryTIReplacedTxs
  | HistoryTIConfirmations
  | HistoryTIBlock
  | HistoryTITime
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
      HistorySend                -> "Send"
      HistoryReceive             -> "Receive"
      HistoryTIBlockUndefined    -> "The transaction is not yet included in a block"
      HistoryTIAddressUndefined  -> "Failed to decode address"
      HistoryTIFeeUndefined      -> "Unknown"
      HistoryTITitle             -> "Transaction info"
      HistoryTIAmount            -> "Amount"
      HistoryTIWalletChanges     -> "Wallet changes"
      HistoryTIHash              -> "Hash"
      HistoryTITransactionId     -> "Transaction id"
      HistoryTILabel             -> "Label"
      HistoryTIURL               -> "Block Explorer"
      HistoryTIFee               -> "Fee"
      HistoryTIRbf               -> "Replace by fee"
      HistoryTIConflictingTxs    -> "Conflicting transactions"
      HistoryTIReplacedTxs       -> "Replaced transactions"
      HistoryTIConfirmations     -> "Confirmations"
      HistoryTIBlock             -> "Block"
      HistoryTITime              -> "Time"
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
      HistorySend                -> "Отправить"
      HistoryReceive             -> "Получить"
      HistoryTIBlockUndefined    -> "Транзакция еще не включена в блок"
      HistoryTIAddressUndefined  -> "Не удалось декодировать адрес"
      HistoryTIFeeUndefined      -> "Неизвестно"
      HistoryTITitle             -> "Информация о транзакции"
      HistoryTIAmount            -> "Объем"
      HistoryTIWalletChanges     -> "Изменения в кошельке"
      HistoryTIHash              -> "Хэш"
      HistoryTITransactionId     -> "Id транзакции"
      HistoryTILabel             -> "Описание"
      HistoryTIURL               -> "Block Explorer"
      HistoryTIFee               -> "Комиссия"
      HistoryTIRbf               -> "Replace by fee"
      HistoryTIConflictingTxs    -> "Конфликтующие транзакции"
      HistoryTIReplacedTxs       -> "Замененные транзакции"
      HistoryTIConfirmations     -> "Подтверждения"
      HistoryTIBlock             -> "Блок"
      HistoryTITime              -> "Время"
      HistoryTIOutputs           -> "Выходы"
      HistoryTIInputs            -> "Входы"
      HistoryTIOutputsAddress    -> "Адрес"
      HistoryTIOutputsOurAddress -> "Наш адрес"
      HistoryTIOutputsValue      -> "Объем"
      HistoryTIOutputsStatus     -> "Статус"
