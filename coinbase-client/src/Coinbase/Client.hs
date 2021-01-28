module Coinbase.Client
  (
    CoinbasePublicAPIEndpoint(..)
  , coinbaseRequest
  , coinbaseRequestMaybe
  , coinbaseReqMultipleRates
  ) where

import Data.Aeson.Lens        (key, _String)
import Control.Lens           ((^?), (&), (.~))
import Control.Monad          (join)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe             (fromMaybe, catMaybes)
import Text.Read              (readMaybe)
import Network.Wreq

import Ergvein.Types.Currency

import qualified Data.Map.Strict as M
import qualified Data.Text as T

coinbaseAPIPath :: String
coinbaseAPIPath = "https://api.coinbase.com/v2/"

-- Two endpoints are commented out for the sake of uniformity
-- All other endpoints result in a Double value
data CoinbasePublicAPIEndpoint =
    ExchangeRate
  | BuyPrice
  | SellPrice
  | SpotPrice
  -- | GetCurrencies
  -- | CurrentTime

endpointUrl :: CoinbasePublicAPIEndpoint -> Currency -> Fiat -> String
endpointUrl endpoint c f = coinbaseAPIPath <> case endpoint of
  ExchangeRate  -> "exchange-rates"
  BuyPrice      -> "prices/" <> show c <> "-" <> show f <> "/buy"
  SellPrice     -> "prices/" <> show c <> "-" <> show f <> "/sell"
  SpotPrice     -> "prices/" <> show c <> "-" <> show f <> "/spot"
  -- GetCurrencies   -> "currencies"
  -- CurrentTime     -> "time"

coinbaseRequestMaybe :: MonadIO m
  => CoinbasePublicAPIEndpoint -> Currency -> Fiat -> m (Maybe Double)
coinbaseRequestMaybe endpoint c f = do
  r <- liftIO $ getWith opts $ endpointUrl endpoint c f
  pure $ r ^? responseBody . key "data" . l . _String
    & join . fmap (readMaybe . T.unpack)
  where
    opts = case endpoint of
      ExchangeRate -> defaults & param "currency" .~ [showt c]
      _ -> defaults
    l = case endpoint of
      ExchangeRate -> key "rates" . key (showt f)
      _  -> key "amount"

coinbaseRequest :: MonadIO m
  => CoinbasePublicAPIEndpoint -> Currency -> Fiat -> m Double
coinbaseRequest e c f = fmap (fromMaybe 0) $ coinbaseRequestMaybe e c f

coinbaseReqMultipleRates :: MonadIO m
 => Currency -> [Fiat] -> m (M.Map Fiat Double)
coinbaseReqMultipleRates c fs = do
  r <- liftIO $ getWith opts $ coinbaseAPIPath <> "exchange-rates"
  pure $ M.fromList $ catMaybes $ extract r <$> fs
  where
    opts = defaults & param "currency" .~ [showt c]
    l f = key "data" . key "rates" . key (showt f)
    extract r f = r ^? responseBody . l f . _String
      & join . fmap (fmap (f,) . readMaybe . T.unpack)

showt :: Show a => a -> T.Text
showt = T.pack . show
