module Ergvein.Types.Currency(
    Currency(..)
  , allCurrencies
  , currencyResolution
  , currencyResolutionUnit
  , currencyName
  , MoneyUnit
  , Money(..)
  , showMoney
  , showMoneyUnit
  , UnitBTC(..)
  , defUnitBTC
  , allUnitsBTC
  , UnitERGO(..)
  , defUnitERGO
  , allUnitsERGO
  , Units(..)
  , defUnits
  , getUnitBTC
  , getUnitERGO
  ) where

import Data.Flat
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.Text (Text)
import Data.Word
import Ergvein.Aeson
import GHC.Generics
import Text.Printf

import qualified Data.Text as T

-- | Supported currencies
data Currency = BTC | ERGO
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Flat)
$(deriveJSON aesonOptions ''Currency)

instance ToJSONKey Currency where
instance FromJSONKey Currency where

-- | All supported currencies
allCurrencies :: [Currency]
allCurrencies = [minBound .. maxBound]

-- | Display units for BTC
data UnitBTC
  = BTC_BTC
  | BTC_mBTC
  | BTC_uBTC
  | BTC_Satoshi
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

$(deriveJSON aesonOptions ''UnitBTC)
instance ToJSONKey UnitBTC where
instance FromJSONKey UnitBTC where

defUnitBTC :: UnitBTC
defUnitBTC = BTC_BTC

allUnitsBTC :: [UnitBTC]
allUnitsBTC = [minBound .. maxBound]

-- | Display units for ERGO
data UnitERGO
  = ERGO_ERGO
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

$(deriveJSON aesonOptions ''UnitERGO)
instance ToJSONKey UnitERGO where
instance FromJSONKey UnitERGO where

defUnitERGO :: UnitERGO
defUnitERGO = ERGO_ERGO

allUnitsERGO :: [UnitERGO]
allUnitsERGO = [minBound .. maxBound]

-- | Union units
data Units = Units {
    unitBTC   :: Maybe UnitBTC
  , unitERGO  :: Maybe UnitERGO
  } deriving (Eq, Ord, Show, Read, Generic)

$(deriveJSON aesonOptions ''Units)
instance ToJSONKey Units where
instance FromJSONKey Units where

defUnits :: Units
defUnits = Units {
    unitBTC   = Just BTC_BTC
  , unitERGO  = Just ERGO_ERGO
  }

getUnitBTC :: Units -> UnitBTC
getUnitBTC Units{..} = fromMaybe defUnitBTC unitBTC

getUnitERGO :: Units -> UnitERGO
getUnitERGO Units{..} = fromMaybe defUnitERGO unitERGO

-- | Amount of digits after point for currency
currencyResolution :: Currency -> Int
currencyResolution c = currencyResolutionUnit c defUnits
{-# INLINE currencyResolution #-}

currencyResolutionUnit :: Currency -> Units -> Int
currencyResolutionUnit c Units{..} = case c of
  BTC  -> case fromMaybe defUnitBTC unitBTC of
            BTC_BTC     -> 8
            BTC_mBTC    -> 6
            BTC_uBTC    -> 3
            BTC_Satoshi -> 0
  ERGO -> case fromMaybe defUnitERGO unitERGO of
            ERGO_ERGO   -> 9
{-# INLINE currencyResolutionUnit #-}

currencyName :: Currency -> Text
currencyName c = case c of
  BTC -> "Bitcoin"
  ERGO -> "Ergo"
{-# INLINE currencyName #-}

-- | Smallest amount of currency
type MoneyUnit = Word64

-- | Amount of money tagged with specific currency
data Money = Money {
    moneyCurrency :: !Currency
  , moneyValue    :: !MoneyUnit
  } deriving (Eq, Ord, Show, Read, Generic)

-- | Convert to rational number amount of cryptocurrency
moneyToRational :: Money -> Rational
moneyToRational (Money cur amount) = fromIntegral amount % (10 ^ currencyResolution cur)
{-# INLINE moneyToRational #-}

moneyToRationalUnit :: Money -> Units -> Rational
moneyToRationalUnit (Money cur amount) units = fromIntegral amount % (10 ^ currencyResolutionUnit cur units)
{-# INLINE moneyToRationalUnit #-}

-- | Convert a rational number to money value
moneyFromRational :: Currency -> Rational -> Money
moneyFromRational cur amount = Money cur val
  where
    val = fromIntegral . numerator $ amount * (denominator amount % 10 ^ currencyResolution cur)
{-# INLINE moneyFromRational #-}

-- | Print amount of cryptocurrency
showMoney :: Money -> Text
showMoney m@(Money cur _) = T.pack $ printf ("%." <> show (currencyResolution cur) <> "f") (realToFrac (moneyToRational m) :: Double)

showMoneyUnit :: Money -> Units -> Text
showMoneyUnit m@(Money cur _) units =
  T.pack $ printf ("%." <> show (currencyResolutionUnit cur units) <> "f") (realToFrac (moneyToRationalUnit m units) :: Double)
