module Binance.Client
  (
    Symbol(..)
  , getMaybeAveragePrice
  , getMaybeCurrentPrice
  , getCurrentPrice
  , getAveragePrice
  ) where

import Control.Monad.IO.Class
import Binance.Client.Types
import Network.Wreq
import Control.Lens
import Control.Monad
import Data.Maybe

binanceApiPath :: String
binanceApiPath = "https://www.binance.com/api/v3/"

data BinanceAPIEndpoints
  = CurrentAveragePrice
  | SymbolPriceTicker

endpointUrl :: BinanceAPIEndpoints -> String
endpointUrl v = binanceApiPath <> case v of
  CurrentAveragePrice -> "avgPrice"
  SymbolPriceTicker -> "ticker/price"

getMaybeAveragePrice :: MonadIO m => Symbol -> m (Maybe Double)
getMaybeAveragePrice s = do
  let opts = defaults & param "symbol" .~ [encodeSymbol s]
      url = endpointUrl CurrentAveragePrice
  r <- liftIO $ asJSON =<< getWith opts url
  pure $ unAvgPrice <$> r ^. responseBody

getMaybeCurrentPrice :: MonadIO m => Symbol -> m (Maybe Double)
getMaybeCurrentPrice s = do
  let opts = defaults & param "symbol" .~ [encodeSymbol s]
      url = endpointUrl SymbolPriceTicker
  r <- liftIO $ asJSON =<< getWith opts url
  pure $ join $ flip fmap (r ^. responseBody) $ \(LatestPrice s' v) -> if s' == s then Just v else Nothing

getCurrentPrice :: MonadIO m => Symbol -> m Double
getCurrentPrice = fmap (fromMaybe 0) . getMaybeCurrentPrice

getAveragePrice :: MonadIO m => Symbol -> m Double
getAveragePrice = fmap (fromMaybe 0) . getMaybeAveragePrice
