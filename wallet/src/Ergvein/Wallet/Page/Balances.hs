module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Data.Maybe (fromMaybe)

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper

data BalanceTitle = BalanceTitle

instance LocalizedPrint BalanceTitle where
  localizedShow l v = case l of
    English -> case v of
      BalanceTitle  -> "Default wallet"
    Russian -> case v of
      BalanceTitle  -> "Настройки"

balancesPage :: MonadFront t m => m ()
balancesPage = do
  let thisWidget = Just $ pure balancesPage
  menuWidget BalanceTitle thisWidget
  wrapper False $ do
    syncWidget
    historyE <- currenciesList
    void $ nextWidget $ ffor historyE $ \cur -> Retractable {
        retractableNext = historyPage cur
      , retractablePrev = thisWidget
      }

syncWidget :: MonadFront t m => m ()
syncWidget = divClass "currency-wrapper" $ do
  progressD <- getSyncProgress
  void $ widgetHoldDyn $ ffor progressD $ \sp -> case sp of
    ScanSynced -> pure ()
    _ -> divClass "sync-progress" $ localizedText sp

data ScanProgress = ScanDays !Int | ScanHours !Int | ScanSynced

instance LocalizedPrint ScanProgress where
  localizedShow l v = case l of
    English -> case v of
      ScanDays d  -> showt d <> if d == 1 then " day behind..." else " days behind..."
      ScanHours h -> showt h <> if h == 1 then " hour behind..." else " hours behind..."
      ScanSynced  -> "Fully synced!"
    Russian -> case v of
      ScanDays d  -> "Отстаём на " <> showt d <> case (d `mod` 10) of
        1 -> " день..."
        2 -> " дня..."
        3 -> " дня..."
        _ -> " дней..."
      ScanHours h -> "Отстаём на " <> showt h <> case (h `mod` 10) of
        1 -> " час..."
        2 -> " часа..."
        3 -> " часа..."
        _ -> " часов..."
      ScanSynced  -> "Синхронизировано!"

-- | Return current amount of days remaining to scan
getSyncProgress :: MonadFront t m => m (Dynamic t ScanProgress)
getSyncProgress = pure $ pure $ ScanDays 10

currenciesList :: MonadFront t m => m (Event t Currency)
currenciesList = divClass "currency-line" $ divClass "currency-content" $
  fmap leftmost $ traverse currencyLine allCurrencies
  where
    currencyLine cur = do
      (e, _) <- divClass' "currency-content-row" $ do
        bal <- currencyBalance cur
        settings <- getSettings
        let setUs = getSettingsUnits settings
        langD <- getLanguage
        divClass "currency-name"    $ text $ currencyName cur
        divClass "currency-balance" $ dynText $ (\v -> showMoneyUnit v setUs) <$> bal
        divClass "currency-unit"    $ dynText $ " (" <> (ffor2 bal langD $ \(Money cur _) lang -> showUnit cur lang setUs) <> ")"
        divClass "currency-arrow"   $ text "〉"
--        divClass "currency-balance" $ do
--          dynText $ do
--            m <- (\v -> showMoneyUnit v setUs) <$> bal
--            u <- ffor2 bal langD $ \(Money cur _) lang -> showUnit cur lang setUs
--            pure $ m <> " (" <> u <> ") " <> "〉"
      pure $ cur <$ domEvent Click e

    getSettingsUnits = fromMaybe defUnits . settingsUnits

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 1

showUnit :: Currency -> Language -> Units -> Text
showUnit cur lang units =
  case cur of
    BTC  -> localizedShow lang $ getUnitBTC  units
    ERGO -> localizedShow lang $ getUnitERGO units
