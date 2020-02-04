{-# LANGUAGE CPP #-}

module Ergvein.Types.Currency(
    Currency(..)
  , btc
  , btcTest
  , erg
  , ergTest
  , getCurrencyNetwork
  , getCurrencyIndex
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
import Data.String
import Data.Text (Text, pack, unpack)
import Data.Version
import Data.Word
import Ergvein.Aeson
import GHC.Generics
import Network.Haskoin.Block
import Network.Haskoin.Constants
import Network.Haskoin.Keys (KeyIndex)
import Text.Printf
import Text.Read(readMaybe)

import qualified Data.Text as T

version :: Version
version = Version [0,8,0] []

-- | Version of Haskoin Core package.
versionString :: IsString a => a
versionString = fromString (showVersion version)

-- | Ergo network. Symbol: ERG.
-- Warning: only fields marked as "+" are correct.
erg :: Network
erg =
    Network
    { getNetworkName = "erg"                                                            -- +
    , getNetworkIdent = "erg"                                                           -- +
    , getAddrPrefix = 1                                                                 -- +
    , getScriptPrefix = 5                                                               -- -
    , getSecretPrefix = 128                                                             -- -
    , getExtPubKeyPrefix = 0x0488b21e                                                   -- +
    , getExtSecretPrefix = 0x0488ade4                                                   -- +
    , getNetworkMagic = 0xf9beb4d9                                                      -- -
    , getGenesisHeader =                                                                -- -
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1231006505
              0x1d00ffff
              2083236893
            -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    , getMaxBlockSize = 1000000                                                         -- -
    , getMaxSatoshi = 2100000000000000                                                  -- -
    , getHaskoinUserAgent =                                                             -- -
          "/haskoin-btc:" <> versionString <> "/"
    , getDefaultPort = 8333                                                             -- -
    , getAllowMinDifficultyBlocks = False                                               -- -
    , getPowNoRetargetting = False                                                      -- -
    , getPowLimit =                                                                     -- -
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =                                                                   -- -
          ( 227931
          , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8")
    , getBip65Height = 388381                                                           -- -
    , getBip66Height = 363725                                                           -- -
    , getTargetTimespan = 14 * 24 * 60 * 60                                             -- -
    , getTargetSpacing = 10 * 60                                                        -- -
    , getCheckpoints = []                                                               -- -
    , getSeeds =[]                                                                      -- -
    , getBip44Coin = 429                                                                -- +
    , getSigHashForkId = Nothing                                                        -- -
    , getEdaBlockHeight = Nothing                                                       -- -
    , getDaaBlockHeight = Nothing                                                       -- -
    , getSegWit = True                                                                  -- -
    , getCashAddrPrefix = Nothing                                                       -- -
    , getBech32Prefix = Just "bc"                                                       -- -
    , getReplaceByFee = True                                                            -- -
    }

-- | Testnet for Ergo network.
-- Warning: only fields marked as "+" are correct.
ergTest :: Network
ergTest =
    Network
    { getNetworkName = "ergtest"                                                        -- +
    , getNetworkIdent = "ergTest"                                                       -- +
    , getAddrPrefix = 17                                                                -- +
    , getScriptPrefix = 196                                                             -- -
    , getSecretPrefix = 239                                                             -- -
    , getExtPubKeyPrefix = 0x043587cf                                                   -- +
    , getExtSecretPrefix = 0x04358394                                                   -- +
    , getNetworkMagic = 0x0b110907                                                      -- -
    , getGenesisHeader =                                                                -- -
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              486604799
              414098458
            -- Hash 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
    , getMaxBlockSize = 1000000                                                         -- -
    , getMaxSatoshi = 2100000000000000                                                  -- -
    , getHaskoinUserAgent = "/haskoin-btc-test:" <> versionString <> "/"                -- -
    , getDefaultPort = 18333                                                            -- -
    , getAllowMinDifficultyBlocks = True                                                -- -
    , getPowNoRetargetting = False                                                      -- -
    , getPowLimit =                                                                     -- -
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =                                                                   -- -
          ( 21111
          , "0000000023b3a96d3484e5abb3755c413e7d41500f8e2a5c3f0dd01299cd8ef8")
    , getBip65Height = 581885                                                           -- -
    , getBip66Height = 330776                                                           -- -
    , getTargetTimespan = 14 * 24 * 60 * 60                                             -- -
    , getTargetSpacing = 10 * 60                                                        -- -
    , getCheckpoints = []                                                               -- -
    , getSeeds = []                                                                     -- -
    , getBip44Coin = 1                                                                  -- -
    , getSigHashForkId = Nothing                                                        -- -
    , getEdaBlockHeight = Nothing                                                       -- -
    , getDaaBlockHeight = Nothing                                                       -- -
    , getSegWit = True                                                                  -- -
    , getCashAddrPrefix = Nothing                                                       -- -
    , getBech32Prefix = Just "tb"                                                       -- -
    , getReplaceByFee = True                                                            -- -
    }

#ifdef TESTNET
-- | Get network correspondent to a given tag
getCurrencyNetwork :: Currency -> Network
getCurrencyNetwork t = case t of
  BTC -> btcTest
  ERGO -> ergTest

getCurrencyIndex :: Currency -> KeyIndex
getCurrencyIndex = const 1
{-# INLINE getCurrencyIndex #-}

#else
getCurrencyNetwork :: Currency -> Network
getCurrencyNetwork t = case t of
  BTC -> btc
  ERGO -> erg

getCurrencyIndex :: Currency -> KeyIndex
getCurrencyIndex t = case t of
  BTC -> getBip44Coin btc
  ERGO -> getBip44Coin erg
#endif

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
