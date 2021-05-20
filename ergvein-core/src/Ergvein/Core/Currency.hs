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
  ETC.ERGO -> if isTestnet then TERGO else ERGO

currencyCodeToCurrency :: CurrencyCode -> ETC.Currency
currencyCodeToCurrency c = case c of
  BTC -> ETC.BTC
  TBTC -> ETC.BTC
  ERGO -> ETC.ERGO
  TERGO -> ETC.ERGO
  _ -> error "Currency code not implemented"
