{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Data.Maybe (fromMaybe)

import Sepulcas.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Status.Widget
import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Debug

import qualified Data.List as L

balancesPage :: MonadFront t m=> m ()
balancesPage = do
  debugWidget
  walletName <- getWalletName
  title <- localized walletName
  wrapper False title (Just $ pure balancesPage) $ do
    statusBarWidget False BTC -- TODO: rework this when we add ERGO
    currenciesList walletName

currenciesList :: MonadFront t m => Text -> m ()
currenciesList _ = divClass "currency-content" $ do
  s <- getSettings
  ps <- getPubStorage
  let currencies = _pubStorage'activeCurrencies ps
      thisWidget = Just $ pure balancesPage
  if L.length currencies == 1
    then do
      buildE <- getPostBuild
      void $ nextWidget $ ffor buildE $ \_ -> Retractable {
        retractableNext = historyPage $ L.head currencies
      , retractablePrev = Nothing
      }
    else do
      historyE <- leftmost <$> traverse (currencyLine s) currencies
      if (settingsPortfolio s)
        then portfolioWidget
        else pure ()
      void $ nextWidget $ ffor historyE $ \cur -> Retractable {
        retractableNext = historyPage cur
      , retractablePrev = thisWidget
      }
  where
    currencyLine settings cur = do
      (e, _) <- divClass' "currency-row" $ do
        bal <- balancesWidget cur
        let setUs = getSettingsUnits settings
        divClass "currency-name"    $ text $ currencyName cur
        divClass "currency-balance" $ do
          elClass "span" "currency-value" $ dynText $ (\v -> showMoneyUnit v setUs) <$> bal
          elClass "span" "currency-unit"  $ text $ symbolUnit cur setUs
          elClass "span" "currency-arrow" $ text "〉"
      pure $ cur <$ domEvent Click e
    getSettingsUnits = fromMaybe defUnits . settingsUnits
