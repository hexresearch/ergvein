module Ergvein.Wallet.Sync.Widget(
    syncWidget
  ) where

import Data.Maybe (fromJust)
import Ergvein.Text
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Util
import Reflex.Localize

syncWidget :: MonadFront t m => Dynamic t SyncProgress -> m ()
syncWidget progressD = divClass "currency-wrapper" $ do
  void $ widgetHoldDyn $ ffor progressD $ \sp -> case sp of
    Synced -> pure ()
    SyncMeta{..} -> do
      divClass "sync-progress" $ localizedText sp
      case syncMetaStage of
        SyncFilters -> traverse_ (divClass "sync-progress" . localizedText) $ syncProgressBehind sp
        _ -> pure ()
