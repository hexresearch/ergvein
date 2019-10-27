module Ergvein.Wallet.Page.Currencies(
    currenciesPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper
import Reflex.Localize

currenciesPage :: MonadFront t m => m ()
currenciesPage = wrapper False $ do
  syncWidget
  currenciesList
  pure ()

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
      ScanSynced  -> "Synced!"
    Russian -> case v of
      ScanDays d  -> "Остаём на " <> showt d <> case (d `mod` 10) of
        1 -> " день..."
        2 -> " дня..."
        3 -> " дня..."
        _ -> " дней..."
      ScanHours h -> "Остаём на " <> showt h <> case (h `mod` 10) of
        1 -> " час..."
        2 -> " часа..."
        3 -> " часа..."
        _ -> " часов..."
      ScanSynced  -> "Синхронизировано!"

-- | Return current amount of days remaining to scan
getSyncProgress :: MonadFront t m => m (Dynamic t ScanProgress)
getSyncProgress = pure $ pure $ ScanDays 10

currenciesList :: MonadFront t m => m ()
currenciesList = traverse_ currencyLine allCurrencies
  where
    currencyLine cur = divClass "currency-wrapper" $ divClass "currency-line" $ do
      divClass "currency-name" $ text $ currencyName cur
      divClass "currency-balance" $ do
        bal <- currencyBalance cur
        dynText $ showMoney <$> bal

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 1
