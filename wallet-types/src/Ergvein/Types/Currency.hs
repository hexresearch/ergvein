module Ergvein.Types.Currency (
    Currency(..)
  , allCurrencies
  , btcResolution
  , ergoResolution
  , currencyResolution
  , currencyResolutionUnit
  , currencyName
  , currencyGenesisTime
  , currencyBlockDuration
  , currencyBlockTime
  , currencyBehind
  , btcSymbolUnit
  , ergoSymbolUnit
  , symbolUnit
  , MoneyUnit
  , Money(..)
  , moneyToRational
  , moneyToRationalUnit
  , moneyFromRational
  , moneyFromRationalUnit
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
  , Fiat(..)
  , allFiats
  , curprefix
  ) where

import Data.Flat
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.SafeCopy
import Data.Serialize (Serialize, get, put)
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Ergvein.Aeson
import Text.Printf

import qualified Data.Text as T

-- | Supported currencies
data Currency = BTC | ERGO
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Flat, Serialize)
$(deriveJSON aesonOptions ''Currency)

instance SafeCopy Currency where
  putCopy = contain . put
  getCopy = contain get

instance ToJSONKey Currency where
instance FromJSONKey Currency where

-- | All supported currencies
allCurrencies :: [Currency]
allCurrencies = [minBound .. maxBound]

-- | Supported fiat
data Fiat = USD | EUR | RUB
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Flat, Serialize)
$(deriveJSON aesonOptions ''Fiat)

instance ToJSONKey Fiat where
instance FromJSONKey Fiat where

-- | All supported currencies
allFiats :: [Fiat]
allFiats = [minBound .. maxBound]

-- | Display units for BTC
data UnitBTC
  = BtcWhole
  | BtcMilli
  | BtcSat
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

$(deriveJSON aesonOptions ''UnitBTC)
instance ToJSONKey UnitBTC where
instance FromJSONKey UnitBTC where

defUnitBTC :: UnitBTC
defUnitBTC = BtcWhole

allUnitsBTC :: [UnitBTC]
allUnitsBTC = [minBound .. maxBound]

btcResolution :: UnitBTC -> Int
btcResolution u = case u of
  BtcWhole -> 8
  BtcMilli -> 5
  BtcSat   -> 0
{-# INLINE btcResolution #-}

btcSymbolUnit :: UnitBTC -> Text
btcSymbolUnit u = case u of
  BtcWhole    -> "BTC"
  BtcMilli    -> "mBTC"
  BtcSat      -> "sat"
{-# INLINE btcSymbolUnit #-}

-- | Display units for ERGO
data UnitERGO
  = ErgWhole
  | ErgMilli
  | ErgNano
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

$(deriveJSON aesonOptions ''UnitERGO)
instance ToJSONKey UnitERGO where
instance FromJSONKey UnitERGO where

defUnitERGO :: UnitERGO
defUnitERGO = ErgWhole

allUnitsERGO :: [UnitERGO]
allUnitsERGO = [minBound .. maxBound]

ergoResolution :: UnitERGO -> Int
ergoResolution u = case u of
  ErgWhole -> 9
  ErgMilli -> 6
  ErgNano  -> 0
{-# INLINE ergoResolution #-}

ergoSymbolUnit :: UnitERGO -> Text
ergoSymbolUnit u = case u of
  ErgWhole    -> "ERG"
  ErgMilli    -> "mERG"
  ErgNano     -> "nERG"
{-# INLINE ergoSymbolUnit #-}

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
    unitBTC   = Just BtcWhole
  , unitERGO  = Just ErgWhole
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
  BTC  -> btcResolution $ fromMaybe defUnitBTC unitBTC
  ERGO -> ergoResolution $ fromMaybe defUnitERGO unitERGO
{-# INLINE currencyResolutionUnit #-}

symbolUnit :: Currency -> Units -> Text
symbolUnit cur Units{..} = case cur of
  BTC  -> btcSymbolUnit $ fromMaybe defUnitBTC unitBTC
  ERGO -> ergoSymbolUnit $ fromMaybe defUnitERGO unitERGO

currencyName :: Currency -> Text
currencyName c = case c of
  BTC -> "Bitcoin"
  ERGO -> "Ergo"
{-# INLINE currencyName #-}

-- | Get time of genesis block of currency
currencyGenesisTime :: Currency -> UTCTime
currencyGenesisTime c = case c of
  BTC -> fromEpoch (1231006505 :: Int)
  ERGO -> fromEpoch (1561998777 :: Int)
  where
    fromEpoch = posixSecondsToUTCTime . fromIntegral

-- | Average duration between blocks
currencyBlockDuration :: Currency -> NominalDiffTime
currencyBlockDuration c = case c of
  BTC  -> 600
  ERGO -> 120

-- | Approx time of block
currencyBlockTime :: Currency -> Int -> UTCTime
currencyBlockTime c i = addUTCTime (fromIntegral i * currencyBlockDuration c) $ currencyGenesisTime c

-- | Get approx time we are behind the head
currencyBehind :: Currency -> Int -> Int -> NominalDiffTime
currencyBehind c n total = fromIntegral (total - n) * currencyBlockDuration c

-- | Smallest amount of currency
type MoneyUnit = Word64

-- | Amount of money tagged with specific currency
data Money = Money {
    moneyCurrency :: !Currency
  , moneyAmount   :: !MoneyUnit
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
    val = round $ amount * (10 ^ currencyResolution cur)
{-# INLINE moneyFromRational #-}

moneyFromRationalUnit :: Currency -> Units-> Rational -> Money
moneyFromRationalUnit cur units amount = Money cur val
  where
    val = round $ amount * (10 ^ currencyResolutionUnit cur units)
{-# INLINE moneyFromRationalUnit #-}

-- | Print amount of cryptocurrency
showMoney :: Money -> Text
showMoney m = T.pack $ printf "%f" (realToFrac (moneyToRational m) :: Double)

showMoneyUnit :: Money -> Units -> Text
showMoneyUnit m units = T.pack $ printf "%f" (realToFrac (moneyToRationalUnit m units) :: Double)

curprefix :: Currency -> Text
curprefix cur = case cur of
  BTC ->  "bitcoin://"
  ERGO -> "ergo://"
