module Ergvein.Types.Currency (
    Currency(..)
  , allCurrencies
  , btcResolution
  , ergoResolution
  , currencyResolution
  , currencyName
  , currencyGenesisTime
  , currencyBlockDuration
  , currencyBlockTime
  , currencyBehind
  , MoneyAmount
  , Money(..)
  , moneyToRational
  , moneyToRationalUnit
  , moneyFromRational
  , moneyFromRationalUnit
  , showMoney
  , showMoneyUnit
  , showMoneyRated
  , IsMoneyUnit(..)
  , UnitBTC(..)
  , defUnitBTC
  , allUnitsBTC
  , smallestUnitBTC
  , UnitERGO(..)
  , defUnitERGO
  , allUnitsERGO
  , smallestUnitERGO
  , Fiat(..)
  , allFiats
  , curprefix
  ) where

import Control.DeepSeq
import Data.Fixed hiding (resolution)
import Data.Flat
import Data.Ratio
import Data.SafeCopy
import Data.Serialize (Serialize, get, put)
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Text.Printf

import Ergvein.Aeson
import Sepulcas.Text

import qualified Data.Text as T

-- | Supported currencies
data Currency = BTC | ERGO
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Flat, Serialize)
$(deriveJSON aesonOptions ''Currency)

instance NFData Currency

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

class IsMoneyUnit a where
  unitResolution :: a -> Int
  unitCurrency :: a -> Currency
  unitIsSmallest :: a -> Bool

-- | Display units for BTC
data UnitBTC
  = BtcWhole
  | BtcMilli
  | BtcSat
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

$(deriveJSON aesonOptions ''UnitBTC)
instance ToJSONKey UnitBTC where
instance FromJSONKey UnitBTC where

instance Display UnitBTC where
  display = btcSymbolUnit

instance IsMoneyUnit UnitBTC where
  unitResolution = btcResolution
  unitCurrency _ = BTC
  unitIsSmallest = isSmallestUnitBTC

defUnitBTC :: UnitBTC
defUnitBTC = BtcSat

smallestUnitBTC :: UnitBTC
smallestUnitBTC = BtcSat

isSmallestUnitBTC :: UnitBTC -> Bool
isSmallestUnitBTC BtcSat = True
isSmallestUnitBTC _ = False

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

instance Display UnitERGO where
  display = ergoSymbolUnit

instance IsMoneyUnit UnitERGO where
  unitResolution = ergoResolution
  unitCurrency _ = ERGO
  unitIsSmallest = isSmallestUnitERGO

defUnitERGO :: UnitERGO
defUnitERGO = ErgWhole

smallestUnitERGO :: UnitERGO
smallestUnitERGO = ErgNano

isSmallestUnitERGO :: UnitERGO -> Bool
isSmallestUnitERGO ErgNano = True
isSmallestUnitERGO _ = False

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

-- | Amount of digits after point for currency
currencyResolution :: Currency -> Int
currencyResolution BTC = btcResolution BtcWhole
currencyResolution ERGO = ergoResolution ErgWhole
{-# INLINE currencyResolution #-}

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

-- | Amount of money in smallest units
type MoneyAmount = Word64

-- | Amount of money tagged with specific currency
data Money = Money {
    moneyCurrency :: !Currency
  , moneyAmount   :: !MoneyAmount
  } deriving (Eq, Ord, Show, Read, Generic)

-- | Convert to rational number amount of cryptocurrency in default units
moneyToRational :: Money -> Rational
moneyToRational (Money cur amount) = fromIntegral amount % (10 ^ currencyResolution cur)
{-# INLINE moneyToRational #-}

-- | Convert to rational number amount of cryptocurrency in specified units
moneyToRationalUnit :: (IsMoneyUnit a) => Money -> a -> Rational
moneyToRationalUnit (Money _ amount) units = fromIntegral amount % (10 ^ unitResolution units)
{-# INLINE moneyToRationalUnit #-}

-- | Convert a rational number to money value
moneyFromRational :: Currency -> Rational -> Money
moneyFromRational cur amount = Money cur val
  where
    val = round $ amount * (10 ^ currencyResolution cur)
{-# INLINE moneyFromRational #-}

moneyFromRationalUnit :: (IsMoneyUnit a) => a -> Rational -> Money
moneyFromRationalUnit units amount = Money cur val
  where
    cur = unitCurrency units
    resolution = unitResolution units
    val = round $ amount * (10 ^ resolution)
{-# INLINE moneyFromRationalUnit #-}

-- | Print amount of cryptocurrency in smallest units
showMoney :: Money -> Text
showMoney money = T.pack $ printf "%u" $ moneyAmount money

showMoneyUnit :: (IsMoneyUnit a) => Money -> a -> Text
showMoneyUnit money units = if unitIsSmallest units
  then showMoney money
  else T.pack $ printf "%f" (realToFrac (moneyToRationalUnit money units) :: Double)

showMoneyRated :: Money -> Centi -> Text
showMoneyRated money r = T.pack $ printf "%.2f" $ (realToFrac r :: Double) * realToFrac (moneyToRational money)

-- See BIP 21
curprefix :: Currency -> Text
curprefix cur = case cur of
  BTC ->  "bitcoin://"
  ERGO -> "ergo://"
