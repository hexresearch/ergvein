{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localize.Validate(
    InvalidAddress(..)
  ) where

import Reflex.Localize
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
      MustBeNatural             -> "Enter a valid natural number"
      MustBeGreaterThan s       -> "Value must be greater than " <> s
      VErrorOther x             -> localizedShow English x
    Russian -> case v of
      MustNotBeEmpty            -> "Заполните это поле"
      MustBeRational            -> "Введите корректное значение (пример: 1.23)"
      MustBeNatural             -> "Введите корректное натуральное число"
      MustBeGreaterThan s       -> "Значение должно быть больше " <> s
      VErrorOther x             -> localizedShow Russian x
