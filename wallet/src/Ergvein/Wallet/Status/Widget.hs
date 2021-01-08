module Ergvein.Wallet.Status.Widget(
    statusBarWidget
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Status.Types

statusBarWidget :: MonadFront t m => Bool -> Currency -> m ()
statusBarWidget isVerbose cur = divClass "sync-widget-wrapper" $ do
  progressD <- getStatusUpdates cur
  langD <- getLanguage
  void $ dynText $ do
    lang <- langD
    sp <- progressD
    pure $ if isVerbose
      then localizedShow lang $ CurrencyStatus cur sp
      else localizedShow lang sp
