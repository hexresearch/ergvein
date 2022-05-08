{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Localize.History
  (
    HistoryPageStrings(..)
  , HistoryTitle(..)
  , BumpFeeWidgetStrings(..)
  , RemoveTxWidgetStrings(..)
  ) where

import Data.Word

import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Sepulcas.Text

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
  | HistorySeedBackupRequired
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
  | HistoryTIBumpFeeBtn
  | HistoryTIRemoveBtn
  | HistoryTIConflictingTxs
  | HistoryTIReplacedTxs
  | HistoryTIPossiblyReplacedTxs
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
      HistoryBalance               -> "Balance"
      HistoryNoTxs                 -> "No transactions yet"
      HistoryUnconfirmed           -> "Unconfirmed"
      HistoryUnconfirmedParents    -> "Unconfirmed parents"
      HistorySend                  -> "Send"
      HistoryReceive               -> "Receive"
      HistorySeedBackupRequired    -> "Backup your wallet"
      HistoryTIBlockUndefined      -> "The transaction is not yet included in a block"
      HistoryTIAddressUndefined    -> "Failed to decode address"
      HistoryTIFeeUndefined        -> "Unknown"
      HistoryTITitle               -> "Transaction info"
      HistoryTIAmount              -> "Amount"
      HistoryTIWalletChanges       -> "Balance change"
      HistoryTIHash                -> "Hash"
      HistoryTITransactionId       -> "Transaction id"
      HistoryTILabel               -> "Label"
      HistoryTIURL                 -> "Block Explorer"
      HistoryTIFee                 -> "Fee"
      HistoryTIRbf                 -> "Replace by fee"
      HistoryTIBumpFeeBtn          -> "Bump fee"
      HistoryTIRemoveBtn           -> "Remove"
      HistoryTIConflictingTxs      -> "Conflicting transactions"
      HistoryTIReplacedTxs         -> "Replaced transactions"
      HistoryTIPossiblyReplacedTxs -> "The transaction may have replaced these transactions or was replaced by one of them"
      HistoryTIConfirmations       -> "Confirmations"
      HistoryTIBlock               -> "Block"
      HistoryTITime                -> "Time"
      HistoryTIOutputs             -> "Outputs"
      HistoryTIInputs              -> "Inputs"
      HistoryTIOutputsAddress      -> "Address"
      HistoryTIOutputsOurAddress   -> "Our address"
      HistoryTIOutputsValue        -> "Value"
      HistoryTIOutputsStatus       -> "Status"

    Russian -> case v of
      HistoryBalance               -> "Баланс"
      HistoryNoTxs                 -> "Транзакций нет"
      HistoryUnconfirmed           -> "Не подтверждена"
      HistoryUnconfirmedParents    -> "Родительские транзакции не подтверждены"
      HistorySend                  -> "Отправить"
      HistoryReceive               -> "Получить"
      HistorySeedBackupRequired    -> "Cделайте резервную копию кошелька"
      HistoryTIBlockUndefined      -> "Транзакция еще не включена в блок"
      HistoryTIAddressUndefined    -> "Не удалось декодировать адрес"
      HistoryTIFeeUndefined        -> "Неизвестно"
      HistoryTITitle               -> "Информация о транзакции"
      HistoryTIAmount              -> "Объем"
      HistoryTIWalletChanges       -> "Изменение баланса"
      HistoryTIHash                -> "Хэш"
      HistoryTITransactionId       -> "Id транзакции"
      HistoryTILabel               -> "Описание"
      HistoryTIURL                 -> "Block Explorer"
      HistoryTIFee                 -> "Комиссия"
      HistoryTIRbf                 -> "Replace by fee"
      HistoryTIBumpFeeBtn          -> "Увеличить комиссию"
      HistoryTIRemoveBtn           -> "Удалить"
      HistoryTIConflictingTxs      -> "Конфликтующие транзакции"
      HistoryTIReplacedTxs         -> "Замененные транзакции"
      HistoryTIPossiblyReplacedTxs -> "Эта транзакция заменила следующие транзакции или была заменена одной из них"
      HistoryTIConfirmations       -> "Подтверждения"
      HistoryTIBlock               -> "Блок"
      HistoryTITime                -> "Время"
      HistoryTIOutputs             -> "Выходы"
      HistoryTIInputs              -> "Входы"
      HistoryTIOutputsAddress      -> "Адрес"
      HistoryTIOutputsOurAddress   -> "Наш адрес"
      HistoryTIOutputsValue        -> "Объем"
      HistoryTIOutputsStatus       -> "Статус"

data BumpFeeWidgetStrings =
    BumpFeeTitle
  | BumpFeeConfirmTxHeader
  | BumpFeeTxPostedHeader
  | BumpFeeCurrentFee
  | BumpFeeCurrentFeeRate
  | BumpFeeDecodeOutsError
  | BumpFeeFeeAmount !Money !UnitBTC
  | BumpFeeFeeRateAmount !Rational
  | BumpFeeFeeRateUnknown
  | BumpFeeFeeUnknown
  | BumpFeeGetChangeKeyError
  | BumpFeeGetChangeOutTypeError
  | BumpFeeGetOutTypeError
  | BumpFeeGetUtxoError
  | BumpFeeHeader
  | BumpFeeInsufficientFundsError
  | BumpFeeNewFee
  | BumpFeeNewFeeRate
  | BumpFeeNewFeeRateAmount !Word64
  | BumpFeeNewFeeRateUnits
  | BumpFeeSendTx
  | BumpFeeSignError
  | BumpFeeSignTx
  | BumpFeeTxId
  deriving (Eq)

instance LocalizedPrint BumpFeeWidgetStrings where
  localizedShow l v = case l of
    English -> case v of
      BumpFeeConfirmTxHeader             -> "Confirm the transaction"
      BumpFeeTxPostedHeader              -> "Transaction posted"
      BumpFeeCurrentFee                  -> "Current fee"
      BumpFeeCurrentFeeRate              -> "Current fee rate"
      BumpFeeDecodeOutsError             -> "Couldn't decode outputs"
      BumpFeeFeeAmount amount units      -> showMoneyUnit amount units <> " " <> display units
      BumpFeeFeeRateAmount amount        -> showf 3 (realToFrac amount :: Double) <> " " <> display smallestUnitBTC <> "/vbyte"
      BumpFeeFeeRateUnknown              -> "unknown"
      BumpFeeFeeUnknown                  -> "unknown"
      BumpFeeGetChangeKeyError           -> "Couldn't get change key"
      BumpFeeGetChangeOutTypeError       -> "Couldn't get change output type"
      BumpFeeGetOutTypeError             -> "Couldn't get output type"
      BumpFeeGetUtxoError                -> "Couldn't get UTXO"
      BumpFeeHeader                      -> "Enter new fee rate"
      BumpFeeInsufficientFundsError      -> "Insufficient funds"
      BumpFeeNewFee                      -> "New fee"
      BumpFeeNewFeeRate                  -> "New fee rate"
      BumpFeeNewFeeRateAmount amount     -> showt amount <> " " <> display smallestUnitBTC <> "/vbyte"
      BumpFeeNewFeeRateUnits             -> "New fee rate, sat/vbyte"
      BumpFeeSendTx                      -> "Send tx"
      BumpFeeSignError                   -> "Failed to sign the transaction"
      BumpFeeSignTx                      -> "Sign tx"
      BumpFeeTitle                       -> "Fee bumping"
      BumpFeeTxId                        -> "Transaction ID"

    Russian -> case v of
      BumpFeeConfirmTxHeader             -> "Подтвердите транзакцию"
      BumpFeeTxPostedHeader              -> "Транзакция отправлена"
      BumpFeeCurrentFee                  -> "Текущая комиссия"
      BumpFeeCurrentFeeRate              -> "Текущая комиссия за байт"
      BumpFeeDecodeOutsError             -> "Не удалось декодировать выходы"
      BumpFeeFeeAmount amount units      -> showMoneyUnit amount units <> " " <> display units
      BumpFeeFeeRateAmount amount        -> showf 3 (realToFrac amount :: Double) <> " " <> display smallestUnitBTC <> "/vbyte"
      BumpFeeFeeRateUnknown              -> "неизвестно"
      BumpFeeFeeUnknown                  -> "неизвестно"
      BumpFeeGetChangeKeyError           -> "Не удалось получить ключ для сдачи"
      BumpFeeGetChangeOutTypeError       -> "Не удалось получить тип выхода для сдачи"
      BumpFeeGetOutTypeError             -> "Не удалось получить тип выхода"
      BumpFeeGetUtxoError                -> "Не удалось получить UTXO"
      BumpFeeHeader                      -> "Укажите новый размер комиссии"
      BumpFeeInsufficientFundsError      -> "Недостаточно средств"
      BumpFeeNewFee                      -> "Новая комиссия"
      BumpFeeNewFeeRate                  -> "Новая комиссия за байт"
      BumpFeeNewFeeRateAmount amount     -> showt amount <> " " <> display smallestUnitBTC <> "/vbyte"
      BumpFeeNewFeeRateUnits             -> "Новая комиссия за байт, sat/vbyte"
      BumpFeeSendTx                      -> "Отправить транзакцию"
      BumpFeeSignError                   -> "Не удалось подписать транзакцию"
      BumpFeeSignTx                      -> "Подписать транзакцию"
      BumpFeeTitle                       -> "Увеличение комиссии"
      BumpFeeTxId                        -> "ID транзакции"

data RemoveTxWidgetStrings =
    RemoveTxTitle
  | RemoveTxHeader
  | RemoveTxBody
  | RemoveTxAgree
  | RemoveTxCancel
  deriving (Eq)

instance LocalizedPrint RemoveTxWidgetStrings where
  localizedShow l v = case l of
    English -> case v of
      RemoveTxTitle    -> "Remove tx"
      RemoveTxHeader   -> "Are you sure to remove the transaction?"
      RemoveTxBody     -> "If the transaction is in mempool, it can still appear even after removal. If a miner publishes a block with the transaction, it will appear again in your wallet. You should delete only those transactions that are stale and are not accepted by the network."
      RemoveTxAgree    -> "Delete it"
      RemoveTxCancel   -> "Cancel"

    Russian -> case v of
      RemoveTxTitle    -> "Удалить транзакцию"
      RemoveTxHeader   -> "Вы точно уверены?"
      RemoveTxBody     -> "Если транзакция попала в мемпул, то она всё ещё может подтвердиться. Если майнер опубликует блок с этой транзакцией, то она снова появится в вашем кошельке. Удаляйте только те транзакции, что застряли и точно не находятся в мемпуле."
      RemoveTxAgree    -> "Удалить её"
      RemoveTxCancel   -> "Отмена"