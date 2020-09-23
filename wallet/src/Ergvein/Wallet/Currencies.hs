module Ergvein.Wallet.Currencies(
    ActiveCurrencies(..)
  ) where

import Ergvein.Aeson
import Ergvein.Types.Currency

data ActiveCurrencies = ActiveCurrencies [Currency] deriving (Eq, Show)

$(deriveJSON (aesonOptionsStripPrefix "") ''ActiveCurrencies)
