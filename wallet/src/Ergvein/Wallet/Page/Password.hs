{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Password(
    passwordPage
  , setupLoginPage
  , setupPatternPage
  , askPasswordPage
  ) where

import Control.Monad.IO.Class

import Ergvein.Crypto.Keys     (Mnemonic)
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Password
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Storage.AuthInfo
import Reflex.Localize

passwordPage :: MonadFrontBase t m => Mnemonic -> [Currency] -> m ()
passwordPage mnemonic curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  logPassE <- setupLoginPassword
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo mnemonic curs) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE

setupLoginPage :: MonadFrontBase t m => Mnemonic -> [Currency] -> m ()
setupLoginPage m ac = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText LPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText LPSDescr
  logE <- setupLogin
  logD <- holdDyn "" logE
  nextWidget $ ffor (updated logD) $ \l -> Retractable {
      retractableNext = setupPatternPage m l ac
    , retractablePrev = Just $ pure $ setupLoginPage m ac
    }
  pure ()

setupPatternPage :: MonadFrontBase t m => Mnemonic -> Text -> [Currency] -> m ()
setupPatternPage m l curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PatPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PatPSDescr
  patE <- setupPattern
  let logPassE = fmap (\p -> (l,p)) patE
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo m curs) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE

askPasswordPage :: MonadFrontBase t m => Text -> m (Event t Password)
askPasswordPage name = wrapperSimple True $ do
  divClass "password-ask-title" $ h4 $ localizedText PPSUnlock
#ifdef ANDROID
  askPattern name
#else
  askPassword
#endif
