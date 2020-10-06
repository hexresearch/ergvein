module Ergvein.Wallet.Sync.Widget(
    syncWidget
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Util

syncWidget :: MonadFront t m => Dynamic t SyncProgress -> m ()
syncWidget progressD = divClass "sync-widget-wrapper" $ do
  langD <- getLanguage
  void $ dynText $ do
    lang <- langD
    sp <- progressD
    pure $ localizedShow lang sp
