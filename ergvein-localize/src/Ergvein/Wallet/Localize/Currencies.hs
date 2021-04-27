module Ergvein.Wallet.Localize.Currencies
  (
    CurrenciesPageStrings(..)
  ) where

import Ergvein.Wallet.Language

data CurrenciesPageStrings =
    CurTitle
  | CurOk

instance LocalizedPrint CurrenciesPageStrings where
  localizedShow l v = case l of
    English -> case v of
      CurTitle -> "Select at least one currency for your wallet"
      CurOk    -> "Ok"
    Russian -> case v of
      CurTitle -> "Выберите хотя бы одну валюту для вашего кошелька"
      CurOk    -> "Ок"
