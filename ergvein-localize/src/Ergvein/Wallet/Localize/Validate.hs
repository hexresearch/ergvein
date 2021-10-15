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
      MustBeGreaterThan t       -> "Value must be greater than " <> t
      InvalidAddress            -> "Invalid address"
      EnterFeeRateFirst         -> "First enter the fee rate"
      EnterRecipientFirst       -> "First enter the recipient"

    Russian -> case v of
      MustNotBeEmpty            -> "Заполните это поле"
      MustBeRational            -> "Введите корректное значение (пример: 1.23)"
      MustBeNatural             -> "Введите корректное натуральное число"
      MustBeGreaterThan t       -> "Значение должно быть больше " <> t
      InvalidAddress            -> "Неверный адрес"
      EnterFeeRateFirst         -> "Сначала выберите размер комиссии"
      EnterRecipientFirst       -> "Cначала укажите получателя"
