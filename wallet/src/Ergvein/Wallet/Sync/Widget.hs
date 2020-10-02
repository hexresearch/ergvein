module Ergvein.Wallet.Sync.Widget(
    syncWidget
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Util

syncWidget :: MonadFront t m => Dynamic t SyncProgress -> m ()
syncWidget progressD = divClass "sync-widget-wrapper" $ void $ workflow syncedStage
  where
    syncedStage = Workflow $ do
      let nextE = fforMaybe (updated progressD) $ \sp -> case sp of
            Synced -> Nothing
            SyncMeta{} -> Just $ metaStage sp
      pure ((), nextE)

    metaStage sp0 = Workflow $ do
      let nextE = fforMaybe (updated progressD) $ \sp -> case sp of
            Synced -> Just syncedStage
            SyncMeta{} -> Nothing
      langD <- getLanguage
      spD <- holdDyn sp0 $ updated progressD
      void $ dynText $ do
        lang <- langD
        sp <- spD
        pure $ localizedShow lang sp
      pure ((), nextE)
