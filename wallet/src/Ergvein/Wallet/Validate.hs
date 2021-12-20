module Ergvein.Wallet.Validate (
    toEither
  , validateEvent
  , validateNow
  , validateAmount
  , validateFeeRate
  , validateBtcAddr
  , validateBtcRecipient
  , validateIP
  , ValidationError(..)
  , Validation(..)
  ) where

import Control.Lens
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Word

import Ergvein.Core.IP
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Sepulcas.Text
import Sepulcas.Validate

validateAmount :: (IsMoneyUnit a, Display a) => Word64 -> a -> Text -> Validation [ValidationError] Word64
validateAmount threshold unit x = case validateNonEmptyText x of
  Failure errs -> _Failure # errs
  Success (NonEmptyText result) ->
    let result' = if unitIsSmallest unit
          then
            case validateWord64 result of
              Failure errs' -> _Failure # errs'
              Success res -> _Success # res
          else
            case validateRational result of
              Failure errs' -> _Failure # errs'
              Success res -> _Success # toSmallestUnits res
        fromSmallestUnits a = fromRational $ fromIntegral a % (10 ^ unitResolution unit) :: Double
        toSmallestUnits a = round $ a * (10 ^ unitResolution unit)
    in
      case result' of
        Failure errs'' -> _Failure # errs''
        Success result'' -> case validateGreaterThan result'' threshold (\a -> showFullPrecision (fromSmallestUnits a) <> " " <> display unit) of
          Failure errs''' -> _Failure # errs'''
          Success (LargeEnoughValue _) -> _Success # result''

validateFeeRate :: Currency -> Maybe Word64 -> Text -> Validation [ValidationError] Word64
validateFeeRate cur mPrevFeeRate feeRateText = case validateNonEmptyText feeRateText of
  Failure errs -> _Failure # errs
  Success (NonEmptyText result) -> case validateWord64 result of
    Failure errs' -> _Failure # errs'
    Success result' -> case mPrevFeeRate of
      Nothing -> _Success # result'
      Just prevFeeRate -> case validateGreaterThan result' prevFeeRate printer of
        Failure errs'' -> _Failure # errs''
        Success (LargeEnoughValue result'') -> _Success # result''
  where
    units = case cur of
      BTC -> display smallestUnitBTC
    printer x = showt x <> " " <> units

validateBtcAddr :: Text -> Validation [ValidationError] BtcAddress
validateBtcAddr addrText = case btcAddrFromText addrText of
  Nothing   -> _Failure # [InvalidAddress]
  Just addr -> _Success # addr

validateBtcRecipient :: Text -> Validation [ValidationError] BtcAddress
validateBtcRecipient addrText = case validateNonEmptyText addrText of
  Failure errs -> _Failure # errs
  Success (NonEmptyText nonEmptyAddrText) -> case validateBtcAddr nonEmptyAddrText of
    Failure errs' -> _Failure # errs'
    Success addr -> _Success # addr

validateIP :: Text -> Validation [ValidationError] IP
validateIP ipText = case parseIP ipText of
  Nothing -> _Failure # [InvalidIP]
  Just ip -> _Success # ip
