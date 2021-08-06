module Ergvein.Wallet.Validate (
    toEither
  , validate
  , validateNow
  , validateBtcRecipient
  , validateBtcAmount
  , validateBtcFeeRate
  , VError(..)
  , Validation(..)
  ) where

import Control.Lens
import Data.Ratio ((%))
import Data.Validation hiding (validate)
import Data.Word

import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Wallet.Localize
import Sepulcas.Text
import Sepulcas.Validate

import qualified Data.Text as T

validateBtcAmount :: Word64 -> UnitBTC -> String -> Validation [VError e] Word64
validateBtcAmount threshold unit x = case validateNonEmptyString x of
  Failure errs -> _Failure # errs
  Success (NonEmptyString result) ->
    let result' = case unit of
          BtcSat -> case validateWord64 result of
            Failure errs' -> _Failure # errs'
            Success res -> _Success # res
          _ -> case validateRational result of
            Failure errs' -> _Failure # errs'
            Success res -> _Success # unitsToSat res
        satToUnits a = fromRational $ fromIntegral a % (10 ^ btcResolution unit) :: Double
        unitsToSat a = round $ a * (10 ^ btcResolution unit)
    in
      case result' of
        Failure errs'' -> _Failure # errs''
        Success result'' -> case validateGreaterThan result'' (Just threshold) (\a -> showFullPrecision (satToUnits a) <> " " <> btcSymbolUnit unit) of
          Failure errs''' -> _Failure # errs'''
          Success (GreaterThan _) -> _Success # result''

validateBtcAddr :: String -> Validation [VError InvalidAddress] BtcAddress
validateBtcAddr addrStr = case btcAddrFromString (T.pack addrStr) of
  Nothing   -> _Failure # [VErrorOther InvalidAddress]
  Just addr -> _Success # addr

validateBtcRecipient :: String -> Validation [VError InvalidAddress] BtcAddress
validateBtcRecipient addrStr = case validateNonEmptyString addrStr of
  Failure errs -> _Failure # errs
  Success (NonEmptyString nonEmptyAddrStr) -> case validateBtcAddr nonEmptyAddrStr of
    Failure errs' -> _Failure # errs'
    Success addr -> _Success # addr

validateBtcFeeRate :: Maybe Word64 -> String -> Validation [VError e] Word64
validateBtcFeeRate mPrevFeeRate feeRateStr = case validateNonEmptyString feeRateStr of
  Failure errs -> _Failure # errs
  Success (NonEmptyString result) -> case validateWord64 result of
    Failure errs' -> _Failure # errs'
    Success result' -> case validateGreaterThan result' mPrevFeeRate (\x -> showt x <> " " <> btcSymbolUnit BtcSat) of
        Failure errs'' -> _Failure # errs''
        Success (GreaterThan result'') -> _Success # result''
