module Coinbase.Client
  (
    CoinbasePublicAPIEndpoint(..)
  , coinbaseRequest
  , coinbaseRequestMaybe
  ) where

import Data.Aeson.Lens        (key, _String)
import Control.Lens           ((^?), (&), (.~))
import Control.Monad          (join)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe             (fromMaybe)
import Text.Read              (readMaybe)
import Network.Wreq

import Ergvein.Types.Currency

import qualified Data.Text as T

coinbaseAPIPath :: String
coinbaseAPIPath = "https://api.coinbase.com/v2/"

-- Two endpoins are commented out for the sake of uniformity
-- All other endpoins result in a Double value
data CoinbasePublicAPIEndpoint =
    ExchangeRate Currency Fiat
  | BuyPrice Currency Fiat
  | SellPrice Currency Fiat
  | SpotPrice Currency Fiat
  -- | GetCurrencies
  -- | CurrentTime

endpointUrl :: CoinbasePublicAPIEndpoint -> String
endpointUrl v = coinbaseAPIPath <> case v of
  ExchangeRate _ _ -> "exchange-rates"
  BuyPrice c f  -> "prices/" <> show c <> "-" <> show f <> "/buy"
  SellPrice c f -> "prices/" <> show c <> "-" <> show f <> "/sell"
  SpotPrice c f -> "prices/" <> show c <> "-" <> show f <> "/spot"
  -- GetCurrencies   -> "currencies"
  -- CurrentTime     -> "time"

coinbaseRequestMaybe :: MonadIO m => CoinbasePublicAPIEndpoint -> m (Maybe Double)
coinbaseRequestMaybe req = do
  r <- liftIO $ getWith opts $ endpointUrl req
  pure $ r ^? responseBody . key "data" . l . _String
    & join . fmap (readMaybe . T.unpack)
  where
    opts = case req of
      ExchangeRate c _ -> defaults & param "currency" .~ [showt c]
      _ -> defaults
    l = case req of
      ExchangeRate _ f -> key "rates" . key (showt f)
      _  -> key "amount"

coinbaseRequest :: MonadIO m => CoinbasePublicAPIEndpoint -> m Double
coinbaseRequest = fmap (fromMaybe 0) . coinbaseRequestMaybe

showt :: Show a => a -> T.Text
showt = T.pack . show
