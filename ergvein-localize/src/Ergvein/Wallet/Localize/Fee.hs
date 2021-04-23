module Ergvein.Wallet.Localize.Fee
  (
    FeeStrings(..)
  , BTCFeeMode(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Wallet.Language

data FeeStrings
  = FSRate
  | FSRateDesc FeeLevel
  | FSFee
  | FSInvalid
  | FSNoFees
  | FSRbf Bool

instance LocalizedPrint FeeStrings where
  localizedShow l v = case l of
    English -> case v of
      FSRate -> "Fee rate, sat/vbyte"
      FSRateDesc lvl -> "Estimated confirmation time ~" <> showt (feeTargetBlocks BTC lvl) <> " blocks"
      FSFee -> "Estimated confirmation time is unknown"
      FSInvalid -> "Enter valid integer fee in sat/vbyte"
      FSNoFees -> "Fees not found in the cache. Please enter the fee manually."
      FSRbf True -> "enabled"
      FSRbf False -> "disabled"
    Russian -> case v of
      FSRate -> "Уровень комиссии, sat/vbyte"
      FSRateDesc lvl -> "Ожидаемое время подтверждения ~" <> showt (feeTargetBlocks BTC lvl) <> " блоков"
      FSFee -> "Ожидаемое время подтверждения неизвестно"
      FSInvalid -> "Введите комиссию. Целое число, sat/vbyte"
      FSNoFees -> "Уровень комиссий не найден в кэше. Пожалуйста, введите комиссию вручную."
      FSRbf True -> "вкл."
      FSRbf False -> "выкл."

data BTCFeeMode = BFMLow | BFMMid | BFMHigh | BFMManual
  deriving (Eq, Ord, Enum, Bounded, Show)

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
