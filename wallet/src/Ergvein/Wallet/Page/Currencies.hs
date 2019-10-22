module Ergvein.Wallet.Page.Currencies(
    currenciesPage
  ) where

import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

currenciesPage :: MonadFront t m => m ()
currenciesPage = wrapper False $ do
  syncWidget
  currenciesList
  pure ()

syncWidget :: MonadFront t m => m ()
syncWidget = do
  progressD <- getSyncProgress
  void $ widgetHoldDyn $ ffor progressD $ \case
    ScanDays d -> divClass "sync-progress" $ text $ showt d <> " days behind..."
    ScanHours h -> divClass "sync-progress" $ text $ showt h <> " hours behind..."
    ScanSynced -> pure ()

data ScanProgress = ScanDays !Int | ScanHours !Int | ScanSynced

-- | Return current amount of days remaining to scan
getSyncProgress :: MonadFront t m => m (Dynamic t ScanProgress)
getSyncProgress = pure $ pure $ ScanDays 10

currenciesList :: MonadFront t m => m ()
currenciesList = pure ()
