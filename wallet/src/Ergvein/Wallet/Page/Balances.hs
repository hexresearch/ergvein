{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

-- import Ergvein.Wallet.Debug
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Status.Widget
import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements
import Sepulcas.Text (Display(..))

import qualified Data.List as L

balancesPage :: MonadFront t m=> m ()
balancesPage = do
  walletName <- getWalletName
  title <- localized walletName
  wrapper False title (Just $ pure balancesPage) $ do
    currenciesList walletName

currenciesList :: MonadFront t m => Text -> m ()
currenciesList _ = divClass "currency-content" $ do
  -- s <- getSettings
  ps <- getPubStorage
  let currencies = _pubStorage'activeCurrencies ps
      thisWidget = Just $ pure balancesPage
  if L.length currencies == 1
    then do
      buildE <- getPostBuild
      void $ nextWidget $ ffor buildE $ const
        Retractable
          {retractableNext = historyPage $ L.head currencies,
           retractablePrev = Nothing}
    else do
      historyE <- leftmost <$> traverse currencyLine currencies
      -- if settingsPortfolio s
      --   then portfolioWidget
      --   else pure ()
      void $ nextWidget $ ffor historyE $ \cur -> Retractable {
        retractableNext = historyPage cur
      , retractablePrev = thisWidget
      }
  where
    currencyLine cur = do
      (e, _) <- divClass' "currency-row" $ do
        bal <- balanceWidget cur
        (balance, units) <- case cur of
          BTC -> do
            moneyUnits <- getSettingsUnitBtc
            let balanceText = dynText $ (`showMoneyUnit` moneyUnits) <$> bal
                unitsText = text $ display moneyUnits
            pure (balanceText, unitsText)
          ERGO -> do
            moneyUnits <- getSettingsUnitErg
            let balanceText = dynText $ (`showMoneyUnit` moneyUnits) <$> bal
                unitsText = text $ display moneyUnits
            pure (balanceText, unitsText)
        divClass "currency-details" $ do
          divClass "currency-name" $ text $ currencyName cur
          divClass "currency-balance" $ do
            elClass "span" "currency-value" balance
            elClass "span" "currency-unit" units
        divClass "currency-status" $ do
          currencyStatusWidget cur
      pure $ cur <$ domEvent Click e
