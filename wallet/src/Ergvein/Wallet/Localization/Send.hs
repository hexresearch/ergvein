module Ergvein.Wallet.Localization.Send(
    SendStrings(..)
  , ConfirmationErrorMessage(..)
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Language

data SendStrings
  = SendTitle Currency
  | SendAvailableBalance
  | SendBtnString
  | RecipientString
  | AmountString
  | SendBtnSign
  | SendBtnSend
  | SendBtnBack
  | SSFee
  | SSTotal
  | SSConfirm
  | SSPosted
  | SSTxId
  | SSRbf

instance LocalizedPrint SendStrings where
  localizedShow l v = case l of
    English -> case v of
      SendTitle c -> "Send " <> currencyName c
      SendAvailableBalance -> "Available"
      SendBtnString -> "Send"
      RecipientString -> "Recipient"
      AmountString -> "Amount"
      SendBtnSign -> "Sign tx"
      SendBtnSend -> "Send tx"
      SendBtnBack -> "Back"
      SSFee -> "Fee"
      SSTotal -> "Total"
      SSConfirm -> "Confirm the transaction"
      SSPosted -> "Transaction posted"
      SSTxId -> "Transaction ID"
      SSRbf -> "Replace by fee"
    Russian -> case v of
      SendTitle c -> "Отправить " <> currencyName c
      SendAvailableBalance -> "Доступно"
      SendBtnString -> "Отправить"
      RecipientString -> "Получатель"
      AmountString -> "Сумма"
      SendBtnSign -> "Подписать транзакцию"
      SendBtnSend -> "Отправить транзакцию"
      SendBtnBack -> "Назад"
      SSFee -> "Комиссия"
      SSTotal -> "Итого"
      SSConfirm -> "Подтвердите транзакцию"
      SSPosted -> "Транзакция отправлена"
      SSTxId -> "ID транзакции"
      SSRbf -> "Replace by fee"

data ConfirmationErrorMessage
  = CEMEmptyUTXO
  | CEMNoChangeKey
  | CEMNoSolution
  | CEMSignFail
  | CEMTxBuildFail

instance LocalizedPrint ConfirmationErrorMessage where
  localizedShow l v = case l of
    English -> case v of
      CEMEmptyUTXO    -> "Empty UTXO set"
      CEMNoChangeKey  -> "Failed to get an address for the change"
      CEMNoSolution   -> "No solution found. Probably not enough money"
      CEMSignFail     -> "Failed to sign the transaction"
      CEMTxBuildFail  -> "Failed to build a transaction"
    Russian -> case v of
      CEMEmptyUTXO    -> "Нет непотраченных выходов"
      CEMNoChangeKey  -> "Не смог получить адрес для сдачи"
      CEMNoSolution   -> "Нет решения. Возможно недостаточно денег"
      CEMSignFail     -> "Не удалось подписать транзакцию"
      CEMTxBuildFail  -> "Не удалось создать транзакцию"
