module Ergvein.Types.Currency(
    Currency(..)
  , allCurrencies
  , currencyResolution
  , currencyName
  , MoneyUnit
  , Money(..)
  , showMoney
  ) where

import Data.Ratio
import Data.Text (Text)
import Data.Word
import Ergvein.Aeson
import GHC.Generics
import Text.Printf

import qualified Data.Text as T

-- | Supported currencies
data Currency = BTC | ERGO
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
$(deriveJSON aesonOptions ''Currency)

-- | All supported currencies
allCurrencies :: [Currency]
allCurrencies = [minBound .. maxBound]

-- | Amount of digits after point for currency
currencyResolution :: Currency -> Int
currencyResolution c = case c of
  BTC -> 8
  ERGO -> 9
{-# INLINE currencyResolution #-}

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

-- | Convert a rational number to money value
moneyFromRational :: Currency -> Rational -> Money
moneyFromRational cur amount = Money cur val
  where
    val = fromIntegral . numerator $ amount * (denominator amount % 10 ^ currencyResolution cur)
{-# INLINE moneyFromRational #-}

-- | Print amount of cryptocurrency
showMoney :: Money -> Text
showMoney m@(Money cur _) = T.pack $ printf ("%." <> show (currencyResolution cur) <> "f") (realToFrac (moneyToRational m) :: Double)
