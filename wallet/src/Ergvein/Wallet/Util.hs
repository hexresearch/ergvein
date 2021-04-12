{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ergvein.Wallet.Util(
    networkHoldDyn
  , networkHoldE
  , networkHoldDynE
  , updatedWithInit
  , poke
  , sampleDyn
  , check
  , nothingIf
  , dbgPrintE
  , eventToNextFrame
  , eventToNextFrame'
  , eventToNextFrameN
  , eventToNextFrameN'
  , mergeDyn
  , currencyToCurrencyCode
  , currencyCodeToCurrency
  , splitEither
  , splitFilter
  , switchDyn2
  , mkChunks
  , calcPercentage
  , mapPercentage
  ) where

import Control.Monad.Except
import Data.Word
import Reflex.Dom
import Reflex.Flunky

import Ergvein.Either
import Ergvein.Index.Protocol.Types
import Ergvein.Wallet.Platform

import qualified Ergvein.Types.Currency as ETC

-- | Helper to throw error when predicate is not 'True'
check :: MonadError a m => a -> Bool -> m ()
check a False = throwError a
check _ True = pure ()

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

calcPercentage :: Word64 -> Word64 -> Word64 -> Double
calcPercentage from to now = 100 * (fromIntegral now - fromIntegral from) / (fromIntegral to - fromIntegral from)

-- | Scales and shifts a segment [0, 100] to a segment [from, to]
-- Example :
--
-- >>> mapPercentage (10, 20) 50
-- 15
mapPercentage :: (Double, Double) -> Double -> Double
mapPercentage (from, to) percentage = from + ((to - from) / 100) * percentage
