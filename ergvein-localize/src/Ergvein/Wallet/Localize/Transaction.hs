{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localize.Transaction(
    TransOutputType(..)
  , CoinSelectionError(..)
  , TxCreationError(..)
  ) where

import Ergvein.Core.Transaction
import Ergvein.Text
import Ergvein.Wallet.Language

instance LocalizedPrint TransOutputType where
  localizedShow l v = case l of
    English -> case v of
      TOSpent   -> "Spent"
      TOUnspent -> "Unspent"
      TOUnknown -> "Unknown"
    Russian -> case v of
      TOSpent   -> "Потрачен"
      TOUnspent -> "Не потрачен"
      TOUnknown -> "Неизвестно"

instance LocalizedPrint CoinSelectionError where
  localizedShow l v = case l of
    English -> case v of
      InsufficientFunds -> "Insufficient funds"
      TargetMustBePositive -> "Target must be > 0"
    Russian -> case v of
      InsufficientFunds -> "Недостаточно средств"
      TargetMustBePositive -> "Сумма должна быть > 0"

instance LocalizedPrint TxCreationError where
  localizedShow l v = case l of
    English -> case v of
      InvalidAddress addr -> "Invalid address " <> addr
      InvalidAmount amount -> "Invalid amount " <> showt amount
    Russian -> case v of
      InvalidAddress addr -> "Неправильный адрес " <> addr
      InvalidAmount amount -> "Неправильная сумма " <> showt amount
