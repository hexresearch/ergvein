module Ergvein.Wallet.Localization.Send
  (
    SendStrings(..)
  , BTCFeeMode(..)
  , FeeStrings(..)
  ) where

import Data.Word

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Wallet.Language

data SendStrings
  = SendTitle Currency
  | SendBtnString
  | RecipientString
  | AmountString
  | BtnPasteString
  | BtnScanQRCode

instance LocalizedPrint SendStrings where
  localizedShow l v = case l of
    English -> case v of
      SendTitle c -> "Send " <> currencyName c
      SendBtnString -> "Send"
      RecipientString -> "Recipient"
      AmountString -> "Amount"
      BtnPasteString -> "Paste"
      BtnScanQRCode -> "Scan"
    Russian -> case v of
      SendTitle c -> "Отправить " <> currencyName c
      SendBtnString -> "Отправить"
      RecipientString -> "Получатель"
      AmountString -> "Сумма"
      BtnPasteString -> "Вставить"
      BtnScanQRCode -> "Сканировать"

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
  | FSLevelDesc (FeeLevel, Word64)
  | FSFee Word64
  | FSInvalid
  | FSNoFees

instance LocalizedPrint FeeStrings where
  localizedShow l v = case l of
    English -> case v of
      FSLevel -> "Fee level"
      FSSelect -> "Select fee level"
      FSLevelDesc (lvl,f) -> "~" <> showt f <> " satoshi/vbyte. <" <> showt (feeTargetBlocks BTC lvl) <> " blocks."
      FSFee f -> "~" <> showt f <> " satoshi/vbyte"
      FSInvalid -> "Enter valid integer fee in satoshi/vbyte"
      FSNoFees -> "Fees not found in the cache. Please enter the fee manually."
    Russian -> case v of
      FSLevel -> "Уровень комиссии"
      FSSelect -> "Выберите уровень комиссии"
      FSLevelDesc (lvl,f) -> "~" <> showt f <> " satoshi/vbyte. <" <> showt (feeTargetBlocks BTC lvl) <> " блоков."
      FSFee f -> "~" <> showt f <> " satoshi/vbyte"
      FSInvalid -> "Введите комиссию. Целое число, satoshi/vbyte"
      FSNoFees -> "Уровень комиссий не найден в кэше. Пожалуйста, введите комиссию вручную."
