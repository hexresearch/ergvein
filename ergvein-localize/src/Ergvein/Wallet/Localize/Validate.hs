{-# OPTIONS_GHC -Wno-orphans #-}

module Ergvein.Wallet.Localize.Validate(
  ) where

import Reflex.Localize
import Sepulcas.Validate
import Ergvein.Wallet.Language

instance LocalizedPrint ValidationError where
  localizedShow l v = case l of
    English -> case v of
      MustNotBeEmpty            -> "This field is required"
      MustBeRational            -> "Enter a valid amount (example: 1.23)"
      MustBeNatural             -> "Enter a valid natural number"
      MustBeInteger             -> "Enter a valid integer number"
      MustBeGreaterThan t       -> "Value must be greater than " <> t
      InvalidAddress            -> "Invalid address"
      InvalidIP                 -> "Invalid IP address"
      SendAllErr                -> "To calculate the maximum amount, specify the recipient's address and the fee rate"

    Russian -> case v of
      MustNotBeEmpty            -> "Заполните это поле"
      MustBeRational            -> "Введите корректное значение (пример: 1.23)"
      MustBeNatural             -> "Введите корректное натуральное число"
      MustBeInteger             -> "Введите корректное целое число"
      MustBeGreaterThan t       -> "Значение должно быть больше " <> t
      InvalidAddress            -> "Неверный адрес"
      InvalidIP                 -> "Неверный IP адрес"
      SendAllErr                -> "Для расчёта максимальной суммы укажите адрес получателя и уровень комиссии"
