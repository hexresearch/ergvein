{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Data.Maybe (fromMaybe)

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Page.Send
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Wrapper

import Control.Monad.IO.Class
import Data.Aeson.Lens  (key, nth)
import Control.Lens
import Data.Map.Strict as Map
import Network.Wreq

data BalancesStrings
  = BalancesTitle
  | ButtonSend
  | ButtonReceive

instance LocalizedPrint BalancesStrings where
  localizedShow l v = case l of
    English -> case v of
      BalancesTitle  -> "Default wallet"
      ButtonSend ->  "Send"
      ButtonReceive -> "Receive"
    Russian -> case v of
      BalancesTitle  -> "Стандартный кошелек"
      ButtonSend ->  "Отправить"
      ButtonReceive -> "Получить"

balancesPage :: MonadFront t m => m ()
balancesPage = do
  anon_name <- getWalletName
#ifdef ANDROID
  c <- liftIO $ loadCounter
  liftIO $ saveCounter $ PatternTries $ Map.insert anon_name 0 (patterntriesCount c)
#endif
  wrapper BalancesTitle (Just $ pure balancesPage) False $ divClass "balances-wrapper" $ do
    syncWidget =<< getSyncProgress
    currenciesList anon_name

currenciesList :: MonadFront t m => Text -> m ()
currenciesList name = divClass "currency-content" $ do
  s <- getSettings
  historyE <- leftmost <$> traverse (currencyLine s) (getActiveCurrencies s)
  let lP = settingsPortfolio s
  if lP
    then porfolioCanvas
    else pure ()
--  r <- liftIO $ get tempErgoUrl
--  let f = r ^? responseBody . key "tickers" . _String
--  divClass "test" $ text $ showt $ f
  --getActiveCurrencies s

  let thisWidget = Just $ pure balancesPage
  void $ nextWidget $ ffor historyE $ \cur -> Retractable {
    retractableNext = historyPage cur
  , retractablePrev = thisWidget
  }
  where
    currencyLine settings cur = do
      (e, _) <- divClass' "currency-row" $ do
        bal <- currencyBalance cur
        let setUs = getSettingsUnits settings
        langD <- getLanguage
        divClass "currency-name"    $ text $ currencyName cur
        divClass "currency-balance" $ do
          elClass "span" "currency-value" $ dynText $ (\v -> showMoneyUnit v setUs) <$> bal
          elClass "span" "currency-unit"  $ dynText $ ffor2 bal langD $ \(Money cur _) lang -> symbolUnit cur lang setUs
          elClass "span" "currency-arrow" $ text "〉"
      pure $ cur <$ domEvent Click e
    getActiveCurrencies s = fromMaybe allCurrencies $ Map.lookup name $ activeCurrenciesMap $ settingsActiveCurrencies s
    getSettingsUnits = fromMaybe defUnits . settingsUnits

tempErgoUrl :: String
tempErgoUrl = "urltoapi"

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 1

symbolUnit :: Currency -> Language -> Units -> Text
symbolUnit cur lang units =
  case cur of
    BTC  -> case getUnitBTC units of
              BtcWhole    -> "btc"
              BtcMilli    -> "mbtc"
              BtcSat      -> "sat"
    ERGO -> case getUnitERGO units of
              ErgWhole    -> "erg"
              ErgMilli    -> "merg"
              ErgNano     -> "nerg"
