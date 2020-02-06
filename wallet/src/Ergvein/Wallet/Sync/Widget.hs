module Ergvein.Wallet.Sync.Widget(
    syncWidget
  ) where

import Ergvein.Wallet.Monad
import Ergvein.Wallet.Sync.Status 
import Ergvein.Wallet.Util 
import Reflex.Localize 

syncWidget :: MonadFront t m => Dynamic t SyncProgress -> m ()
syncWidget progressD = divClass "currency-wrapper" $ do
  void $ widgetHoldDyn $ ffor progressD $ \sp -> case sp of
    Synced -> pure ()
    _ -> divClass "sync-progress" $ localizedText sp