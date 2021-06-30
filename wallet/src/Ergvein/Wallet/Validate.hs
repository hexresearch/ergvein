module Ergvein.Wallet.Validate (
    toEither
  , validateEvent
  , validateNow
  , validateAmount
  , validateFeeRate
  , validateBtcAddr
  , validateErgAddr
  , validateBtcRecipient
  , validateErgRecipient
  , ValidationError(..)
  , Validation(..)
  ) where

import Control.Lens
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Word

import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Wallet.Localize
import Sepulcas.Text
import Sepulcas.Validate

import qualified Data.Text as T

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
        fromSmallestUnits x = fromRational $ fromIntegral x % (10 ^ unitResolution unit)
        toSmallestUnits x = round $ x * (10 ^ unitResolution unit)
    in
      case result' of
        Failure errs'' -> _Failure # errs''
        Success result'' -> case validateGreaterThan result'' threshold (\x -> showFullPrecision (fromSmallestUnits x) <> " " <> display unit) of
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
      ERGO -> display smallestUnitERGO
    printer x = showt x <> " " <> units

validateBtcAddr :: Text -> Validation [ValidationError] BtcAddress
validateBtcAddr addrText = case btcAddrFromText addrText of
  Nothing   -> _Failure # [InvalidAddress]
  Just addr -> _Success # addr

validateErgAddr :: Text -> Validation [ValidationError] ErgAddress
validateErgAddr addrText = case ergAddrFromText addrText of
  Nothing   -> _Failure # [InvalidAddress]
  Just addr -> _Success # addr

validateBtcRecipient :: Text -> Validation [ValidationError] BtcAddress
validateBtcRecipient addrText = case validateNonEmptyText addrText of
  Failure errs -> _Failure # errs
  Success (NonEmptyText nonEmptyAddrText) -> case validateBtcAddr nonEmptyAddrText of
    Failure errs' -> _Failure # errs'
    Success addr -> _Success # addr

validateErgRecipient :: Text -> Validation [ValidationError] ErgAddress
validateErgRecipient addrText = case validateNonEmptyText addrText of
  Failure errs -> _Failure # errs
  Success (NonEmptyText nonEmptyAddrText) -> case validateErgAddr nonEmptyAddrText of
    Failure errs' -> _Failure # errs'
    Success addr -> _Success # addr
