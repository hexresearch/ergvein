module Ergvein.Wallet.Localize.Fee
  (
    FeeStrings(..)
  , FeeMode(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Wallet.Language

data FeeStrings
  = FSRate Currency
  | FSRateDesc FeeLevel
  | FSFee
  | FSNoFees
  | FSRbf Bool

instance LocalizedPrint FeeStrings where
  localizedShow l v = case l of
    English -> case v of
      FSRate cur -> case cur of
        BTC -> "Fee rate, sat/vbyte"
      FSRateDesc lvl -> "Estimated confirmation time ~" <> showt (feeTargetBlocks BTC lvl) <> " blocks"
      FSFee -> "Estimated confirmation time is unknown"
      FSNoFees -> "Fees not found in the cache. Please enter the fee manually."
      FSRbf True -> "enabled"
      FSRbf False -> "disabled"
    Russian -> case v of
      FSRate cur -> case cur of
        BTC -> "Уровень комиссии, sat/vbyte"
      FSRateDesc lvl -> "Ожидаемое время подтверждения ~" <> showt (feeTargetBlocks BTC lvl) <> " блоков"
      FSFee -> "Ожидаемое время подтверждения неизвестно"
      FSNoFees -> "Уровень комиссий не найден в кэше. Пожалуйста, введите комиссию вручную."
      FSRbf True -> "вкл."
      FSRbf False -> "выкл."

data FeeMode = FeeModeLow | FeeModeMid | FeeModeHigh | FeeModeManual
  deriving (Eq, Ord, Enum, Bounded, Show)

instance LocalizedPrint FeeMode where
  localizedShow l v = case l of
    English -> case v of
      FeeModeLow    -> "Low"
      FeeModeMid    -> "Mid"
      FeeModeHigh   -> "High"
      FeeModeManual -> "Manual"
    Russian -> case v of
      FeeModeLow    -> "Низкий"
      FeeModeMid    -> "Средний"
      FeeModeHigh   -> "Высокий"
      FeeModeManual -> "Вручную"
