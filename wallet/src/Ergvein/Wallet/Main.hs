module Ergvein.Wallet.Main(
    frontendUnauth
  , mainWidgetWithCss
  ) where

import Data.ByteString (ByteString)
import Ergvein.Wallet.Alert.Handler
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Log.Writer
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Initial
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Reflex.Dom.Main (mainWidgetWithCss)


frontendUnauth :: MonadFrontBase t m => m ()
frontendUnauth = do
  alertHandlerWidget
  logWriter =<< fmap fst getLogsTrigger
  void $ retractStack initialPage
  -- requestAuth initialPage mnemonicPage
