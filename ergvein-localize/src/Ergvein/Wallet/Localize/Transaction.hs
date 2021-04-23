{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localize.Transaction(
    TransOutputType(..)
  ) where

import Ergvein.Core.Transaction
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
