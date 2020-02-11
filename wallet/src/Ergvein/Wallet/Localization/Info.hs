module Ergvein.Wallet.Localization.Info(
    InfoPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language

import Data.Text

data InfoPageStrings =
    InfoTitle !Currency

instance LocalizedPrint InfoPageStrings where
  localizedShow l v = case l of
    English -> case v of
      InfoTitle c -> "Info " <> currencyName c
    Russian -> case v of
      InfoTitle c -> "Информация " <> currencyName c
