module Ergvein.Wallet.Localization.Send
  (
    SendStrings(..)
  , BTCFeeMode(..)
  , FeeStrings(..)
  , ConfirmationErrorMessage(..)
  ) where

import Data.Word

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees
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
      SendBtnSign -> "Sign Tx"
      SendBtnSend -> "Send Tx"
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
      SSConfirm -> "Обзор транзакции"
      SSPosted -> "Транзакция отправлена"
      SSTxId -> "ID транзакции"
      SSRbf -> "Replace by fee"

data BTCFeeMode = BFMLow | BFMMid | BFMHigh | BFMManual
  deriving (Eq)

instance LocalizedPrint BTCFeeMode where
  localizedShow l v = case l of
    English -> case v of
      BFMLow    -> "Low"
      BFMMid    -> "Mid"
      BFMHigh   -> "High"
      BFMManual -> "Manual"
    Russian -> case v of
      BFMLow    -> "Низкий"
      BFMMid    -> "Средний"
      BFMHigh   -> "Высокий"
      BFMManual -> "Вручную"

data FeeStrings
  = FSLevel
  | FSSelect
  | FSLevelDesc FeeLevel Word64
  | FSFee Word64
  | FSInvalid
  | FSNoFees
  | FSRbf Bool

instance LocalizedPrint FeeStrings where
  localizedShow l v = case l of
    English -> case v of
      FSLevel -> "Fee level"
      FSSelect -> "Select fee level"
      FSLevelDesc lvl f -> "~" <> showt f <> " satoshi/vbyte. <" <> showt (feeTargetBlocks BTC lvl) <> " blocks."
      FSFee f -> "~" <> showt f <> " satoshi/vbyte"
      FSInvalid -> "Enter valid integer fee in satoshi/vbyte"
      FSNoFees -> "Fees not found in the cache. Please enter the fee manually."
      FSRbf True -> "enabled"
      FSRbf False -> "disabled"
    Russian -> case v of
      FSLevel -> "Уровень комиссии"
      FSSelect -> "Выберите уровень комиссии"
      FSLevelDesc lvl f -> "~" <> showt f <> " satoshi/vbyte. <" <> showt (feeTargetBlocks BTC lvl) <> " блоков."
      FSFee f -> "~" <> showt f <> " satoshi/vbyte"
      FSInvalid -> "Введите комиссию. Целое число, satoshi/vbyte"
      FSNoFees -> "Уровень комиссий не найден в кэше. Пожалуйста, введите комиссию вручную."
      FSRbf True -> "вкл."
      FSRbf False -> "выкл."

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
