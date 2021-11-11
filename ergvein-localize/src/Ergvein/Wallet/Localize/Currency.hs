{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localize.Currency(
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Language

instance LocalizedPrint Currency where
  localizedShow _ v = case v of
    BTC   -> "BTC"

instance LocalizedPrint Fiat where
  localizedShow _ v = case v of
    USD -> "USD"
    EUR -> "EUR"
    RUB -> "RUB"
