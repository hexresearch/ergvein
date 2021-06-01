module Ergvein.Wallet.Validate (
    toEither
  , validateEvent
  , validateNow
  , validateBtcAmount
  , validateErgAmount
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

validateBtcAmount :: Word64 -> UnitBTC -> Text -> Validation [ValidationError] Word64
validateBtcAmount threshold unit x = case validateNonEmptyText x of
  Failure errs -> _Failure # errs
  Success (NonEmptyText result) ->
    let result' = case unit of
          BtcSat -> case validateWord64 result of
            Failure errs' -> _Failure # errs'
            Success res -> _Success # res
          _ -> case validateRational result of
            Failure errs' -> _Failure # errs'
            Success res -> _Success # unitsToSat res
        satToUnits x = fromRational $ fromIntegral x % (10 ^ btcResolution unit)
        unitsToSat x = round $ x * (10 ^ btcResolution unit)
    in
      case result' of
        Failure errs'' -> _Failure # errs''
        Success result'' -> case validateGreaterThan result'' threshold (\x -> showFullPrecision (satToUnits x) <> " " <> btcSymbolUnit unit) of
          Failure errs''' -> _Failure # errs'''
          Success (LargeEnoughValue _) -> _Success # result''

validateErgAmount :: Word64 -> UnitERGO -> Text -> Validation [ValidationError] Word64
validateErgAmount threshold unit x = case validateNonEmptyText x of
  Failure errs -> _Failure # errs
  Success (NonEmptyText result) ->
    let result' = case unit of
          ErgNano -> case validateWord64 result of
            Failure errs' -> _Failure # errs'
            Success res -> _Success # res
          _ -> case validateRational result of
            Failure errs' -> _Failure # errs'
            Success res -> _Success # unitsToNanoErgs res
        nanoErgsToUnits x = fromRational $ fromIntegral x % (10 ^ ergoResolution unit)
        unitsToNanoErgs x = round $ x * (10 ^ ergoResolution unit)
    in
      case result' of
        Failure errs'' -> _Failure # errs''
        Success result'' -> case validateGreaterThan result'' threshold (\x -> showFullPrecision (nanoErgsToUnits x) <> " " <> ergoSymbolUnit unit) of
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
    printer x = showt x <> " " <> symbolUnit cur smallestUnits

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
