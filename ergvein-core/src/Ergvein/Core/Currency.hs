module Ergvein.Core.Currency(
    currencyToCurrencyCode
  , currencyCodeToCurrency
  ) where

import Ergvein.Index.Protocol.Types
import Ergvein.Core.Platform

import qualified Ergvein.Types.Currency as ETC

currencyToCurrencyCode :: ETC.Currency -> CurrencyCode
currencyToCurrencyCode c = case c of
  ETC.BTC -> if isTestnet then TBTC else BTC

currencyCodeToCurrency :: CurrencyCode -> ETC.Currency
currencyCodeToCurrency c = case c of
  BTC -> ETC.BTC
  TBTC -> ETC.BTC
  _ -> error "Currency code not implemented"
