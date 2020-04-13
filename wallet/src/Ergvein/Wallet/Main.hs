{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Control.Monad.IO.Class
import Data.Time
import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Alert.Handler
import Ergvein.Wallet.Client
import Ergvein.Wallet.Loading
import Ergvein.Wallet.Log.Writer
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.Initial
import Ergvein.Wallet.Password
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Util
import Ergvein.Wallet.Worker.Info
import Reflex.ExternalRef

import Reflex.Dom.Main (mainWidgetWithCss)

frontend :: MonadFrontBase t m => m ()
frontend = do
  logWrite "Frontend started"
  alertHandlerWidget
  loadingWidget
#ifdef ANDROID
  askPatternModal
#else
  askPasswordModal
#endif
  logWriter =<< fmap fst getLogsTrigger
  logWrite "Entering initial page"
  void $ retractStack initialPage `liftAuth` (accountDiscovery >> retractStack balancesPage)
