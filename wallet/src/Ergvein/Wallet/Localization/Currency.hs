{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localization.Currency(
  ) where

import Ergvein.Core
import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Text

instance LocalizedPrint Currency where
  localizedShow _ v = case v of
    BTC   -> "BTC"
    ERGO  -> "ERGO"

instance LocalizedPrint Fiat where
  localizedShow _ v = case v of
    USD -> "USD"
    EUR -> "EUR"
    RUB -> "RUB"
