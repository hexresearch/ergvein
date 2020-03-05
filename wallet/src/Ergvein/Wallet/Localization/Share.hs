module Ergvein.Wallet.Localization.Share(
    SharePageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language

import Data.Text

data SharePageStrings =
    ShareTitle !Currency
  | ShareLink
  | ShareCopy
  | ShareShare

instance LocalizedPrint SharePageStrings where
  localizedShow l v = case l of
    English -> case v of
      ShareTitle c      -> "Share for " <> currencyName c
      ShareLink         -> "Link: "
      ShareCopy         -> "Copy"
      ShareShare        -> "Share"
    Russian -> case v of
      ShareTitle c      -> "Поделиться для " <> currencyName c
      ShareLink         -> "Ссылка: "
      ShareCopy         -> "Копировать"
      ShareShare        -> "Поделиться"
