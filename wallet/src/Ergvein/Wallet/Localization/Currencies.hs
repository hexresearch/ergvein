module Ergvein.Wallet.Localization.Currencies
  (
    CurrenciesPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

import Data.Text

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
