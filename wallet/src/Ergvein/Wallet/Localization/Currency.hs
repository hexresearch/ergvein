{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localization.Currency(
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Language

instance LocalizedPrint Currency where
  localizedShow l v = case l of
    English -> case v of
      BTC   -> "BTC"
      ERGO  -> "ERGO"
    Russian -> case v of
      BTC   -> "BTC"
      ERGO  -> "ERGO"
