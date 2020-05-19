{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Data.Maybe (fromMaybe)

import Ergvein.Types.Currency
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Wrapper

import Data.Map.Strict as Map
import Network.Wreq

#ifdef ANDROID
import Control.Monad.IO.Class
#endif

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
        divClass "currency-name"    $ text $ currencyName cur
        divClass "currency-balance" $ do
          elClass "span" "currency-value" $ dynText $ (\v -> showMoneyUnit v setUs) <$> bal
          elClass "span" "currency-unit"  $ dynText $ ffor bal $ \(Money c _) -> symbolUnit c setUs
          elClass "span" "currency-arrow" $ text "〉"
      pure $ cur <$ domEvent Click e
    getActiveCurrencies s = fromMaybe allCurrencies $ Map.lookup name $ activeCurrenciesMap $ settingsActiveCurrencies s
    getSettingsUnits = fromMaybe defUnits . settingsUnits

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 0

symbolUnit :: Currency -> Units -> Text
symbolUnit cur units =
  case cur of
    BTC  -> case getUnitBTC units of
              BtcWhole    -> "btc"
              BtcMilli    -> "mbtc"
              BtcSat      -> "sat"
    ERGO -> case getUnitERGO units of
              ErgWhole    -> "erg"
              ErgMilli    -> "merg"
              ErgNano     -> "nerg"
