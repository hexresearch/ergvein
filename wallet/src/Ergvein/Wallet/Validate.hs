module Ergvein.Wallet.Validate (
    toEither
  , validate
  , validateNow
  , validateAmount
  , validateBtcRecipient
  , validateBtcWithUnits
  , validateFeeRate
  , VError(..)
  , Validation(..)
  ) where

import Control.Lens
import Data.Validation hiding (validate)
import Data.Word

import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Wallet.Localize
import Sepulcas.Validate

import qualified Data.Text as T

validateBtcWithUnits :: UnitBTC -> String -> Validation [VError e] Word64
validateBtcWithUnits unit x = case validateNonEmptyString x of
  Failure errs -> _Failure # errs
  Success (NonEmptyString result) -> case unit of
    BtcSat -> case validateWord64 result of
      Failure errs' -> _Failure # errs'
      Success result' -> _Success # result'
    _ -> case validateRational result of
      Failure errs' -> _Failure # errs'
      Success result' -> case validatePositiveRational result' of
        Failure errs'' -> _Failure # errs''
        Success (PositiveRational result'') -> _Success # (floor $ result'' * 10 ^ btcResolution unit)

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

validateFeeRate :: Maybe Rational -> String -> Validation [VError e] Word64
validateFeeRate mFeeRateThreshold feeRateStr = case validateNonEmptyString feeRateStr of
  Failure errs -> _Failure # errs
  Success (NonEmptyString result) -> case validateWord64 result of
    Failure errs' -> _Failure # errs'
    Success result' -> case validateGreaterThan mFeeRateThreshold result' of
        Failure errs'' -> _Failure # errs''
        Success (GreaterThanRational result'') -> _Success # result''
