module Ergvein.Wallet.Localization.PatternKey
  (
    PatternKeyStrings(..)
  ) where

import Ergvein.Wallet.Language

data PatternKeyStrings = PKSFirstTry | PKSSecondTry | PKSErrorTry | PKSDone

instance LocalizedPrint PatternKeyStrings where
  localizedShow l v = case l of
    English -> case v of
      PKSFirstTry -> "Enter pattern key."
      PKSSecondTry -> "Repeat pattern key. Keys should match."
      PKSErrorTry -> "Keys don't match. Enter pattern key."
      PKSDone -> "Keys match."
    Russian -> case v of
      PKSFirstTry -> "Введите графический ключ."
      PKSSecondTry -> "Повторите графический ключ. Ключи должны совпадать."
      PKSErrorTry -> "Ключи не совпадают. Введите графический ключ."
      PKSDone -> "Ключи совпадают."
