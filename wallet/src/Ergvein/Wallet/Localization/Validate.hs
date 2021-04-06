module Ergvein.Wallet.Localization.Validate(
    InvalidAddress(..)
  ) where

import Reflex.Localize
import Sepulcas.Text
import Sepulcas.Validate
import Ergvein.Wallet.Language

data InvalidAddress = InvalidAddress

instance LocalizedPrint InvalidAddress where
  localizedShow l v = case l of
    English -> case v of
      InvalidAddress            -> "Invalid address"
    Russian -> case v of
      InvalidAddress            -> "Неверный адрес"

instance LocalizedPrint ext => LocalizedPrint (VError ext) where
  localizedShow l v = case l of
    English -> case v of
      MustNotBeEmpty            -> "This field is required"
      MustBeRational            -> "Enter a valid amount (example: 1.23)"
      MustBeNonNegativeIntegral -> "Enter a valid non-negative integer"
      MustBePositive            -> "Value must be positive"
      MustBeGreaterThan x       -> "Value must be greater than " <> showf 3 (realToFrac x :: Double)
      VErrorOther x             -> localizedShow English x
    Russian -> case v of
      MustNotBeEmpty            -> "Заполните это поле"
      MustBeRational            -> "Введите корректное значение (пример: 1.23)"
      MustBeNonNegativeIntegral -> "Введите корректное неотрицательное целочисленное значение"
      MustBePositive            -> "Значение должно быть положительным"
      MustBeGreaterThan x       -> "Значение должно быть больше " <> showf 3 (realToFrac x :: Double)
      VErrorOther x             -> localizedShow Russian x
