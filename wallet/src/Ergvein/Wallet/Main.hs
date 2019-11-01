module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Data.ByteString (ByteString)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Error
import Ergvein.Wallet.Log.Writer
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Initial
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Reflex.Dom.Main (mainWidgetWithCss)

import Ergvein.Wallet.Page.Currencies
import Ergvein.Wallet.Page.Seed

frontend :: MonadFront t m => m ()
frontend = do
  errorHandlerWidget
  logWriter =<< fmap fst getLogsTrigger
  void $ retractStack initialPage
