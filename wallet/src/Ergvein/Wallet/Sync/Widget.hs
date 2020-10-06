module Ergvein.Wallet.Sync.Widget(
    syncWidget
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Util

syncWidget :: MonadFront t m => Currency -> m ()
syncWidget cur = divClass "sync-widget-wrapper" $ do
  progressD <- getSyncProgress cur
  langD <- getLanguage
  void $ dynText $ do
    lang <- langD
    sp <- progressD
    pure $ localizedShow lang $ SyncProgress cur sp
