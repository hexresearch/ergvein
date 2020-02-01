{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Data.ByteString (ByteString)
import Ergvein.Wallet.Alert.Handler
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Loading
import Ergvein.Wallet.Log.Writer
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.Initial
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Password
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Reflex.Dom.Main (mainWidgetWithCss)

import Ergvein.Wallet.Native

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
  void $ retractStack initialPage `liftAuth` retractStack balancesPage
