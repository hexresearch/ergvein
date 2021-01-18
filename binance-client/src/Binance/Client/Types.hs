module Binance.Client.Types
  (
    Symbol(..)
  , encodeSymbol
  , decodeSymbol
  , AvgPrice(..)
  , LatestPrice(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Text.Read
import qualified Data.Text as T

data Symbol = BTCUSDT | BTCBUSD
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

newtype AvgPrice = AvgPrice { unAvgPrice :: Double }
  deriving (Eq, Show)

data LatestPrice = LatestPrice {
  latestSymbol  :: !Symbol
, latestVal     :: !Double
} deriving (Eq, Show)

encodeSymbol :: Symbol -> Text
encodeSymbol = T.toUpper . T.pack . show

decodeSymbol :: Text -> Maybe Symbol
decodeSymbol t = case T.toUpper t of
  "BTCUSDT" -> Just BTCUSDT
  "BTCBUSD" -> Just BTCBUSD
  _ -> Nothing

instance FromJSON Symbol where
  parseJSON = withText "Symbol" $ \t -> maybe (fail $ "Unknown Symbol: " <> T.unpack t) pure . decodeSymbol $ t

instance FromJSON AvgPrice where
  parseJSON = withObject "AvgPrice" $ \o -> do
    mp <- o .: "price"
    maybe (fail $ "Failed to parse as Double: " <> mp) (pure . AvgPrice) $ readMaybe mp

instance FromJSON LatestPrice where
  parseJSON = withObject "AvgPrice" $ \o -> do
    s  <- o .: "symbol"
    mp <- o .: "price"
    maybe (fail $ "Failed to parse as Double: " <> mp) (pure . LatestPrice s) $ readMaybe mp
