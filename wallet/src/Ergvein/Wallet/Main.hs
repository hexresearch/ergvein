module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Control.Monad.IO.Class
import Data.Time
import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Alert.Handler
import Ergvein.Wallet.Client
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Loading
import Ergvein.Wallet.Localization.TestnetDisclaimer
import Ergvein.Wallet.Log.Writer
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.Initial
import Ergvein.Wallet.Page.Restore
import Ergvein.Wallet.Password
import Ergvein.Wallet.Util
import Ergvein.Wallet.Wrapper
import Reflex.ExternalRef

import Reflex.Dom.Main (mainWidgetWithCss)

frontend :: MonadFrontBase t m => m ()
frontend = do
  logWrite "Frontend started"
  loadingWidget
  askPasswordModal
  logWriter =<< fmap fst getLogsTrigger
  logWrite "Entering initial page"
  testnetDisclaimerPage

startPage :: MonadFront t m => m ()
startPage = do
  ps <- getPubStorage
  if _pubStorage'restoring ps
    then restorePage
    else balancesPage

-- TODO: remove this disclaimer when ERGO is ready
testnetDisclaimerPage :: MonadFrontBase t m => m ()
testnetDisclaimerPage = void $ workflow testnetDisclaimer
  where
    testnetDisclaimer = Workflow $ wrapperSimple True $ do
      elClass "h4" "testnet-disclaimer-label" $ dynText =<< localized TestnetDisclaimerLabel
      elClass "p" "testnet-disclaimer-text" $ dynText =<< localized TestnetDisclaimerText
      closeE <- outlineButton TestnetDisclaimerClose
      pure ((), startWallet <$ closeE)
    startWallet = Workflow $ do
      void $ retractStack initialPage `liftAuth` retractStack startPage
      pure ((), never)
