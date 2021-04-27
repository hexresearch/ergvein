module Ergvein.Wallet.Localize.PatternKey
  (
    PatternKeyStrings(..)
  ) where

import Data.Text (Text)
import Ergvein.Wallet.Language

data PatternKeyStrings = PKSFirstTry | PKSSecondTry | PKSErrorTry | PKSDone | PKSAsk | PKSUnlock | PKSFor Text

instance LocalizedPrint PatternKeyStrings where
  localizedShow l v = case l of
    English -> case v of
      PKSFirstTry -> "Enter pattern key."
      PKSSecondTry -> "Repeat pattern key. Keys should match."
      PKSErrorTry -> "Keys don't match. Enter pattern key."
      PKSDone -> "Keys match."
      PKSAsk -> "Sign settings changes by your key."
      PKSUnlock -> "Enter the pattern to unlock private storage"
      PKSFor n -> "Pattern for " <> n
    Russian -> case v of
      PKSFirstTry -> "Введите графический ключ."
      PKSSecondTry -> "Повторите графический ключ. Ключи должны совпадать."
      PKSErrorTry -> "Ключи не совпадают. Введите графический ключ."
      PKSDone -> "Ключи совпадают."
      PKSAsk -> "Подпишите изменения настроек своим ключом."
      PKSUnlock -> "Введите графический ключ для расшифровки приватного хранилища"
      PKSFor n -> "Графический ключ для " <> n
