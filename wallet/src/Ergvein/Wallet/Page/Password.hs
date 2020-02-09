{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Password(
    passwordPage
  , setupLoginPage
  , setupPatternPage
  , askPasswordPage
  ) where

import Ergvein.Crypto.Keys     (Mnemonic)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Password
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Storage.AuthInfo
import Reflex.Localize

passwordPage :: MonadFrontBase t m => Mnemonic -> m ()
passwordPage mnemonic = wrapper True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  logPassE <- setupLoginPassword
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo mnemonic) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE

setupLoginPage :: MonadFrontBase t m => Mnemonic -> m ()
setupLoginPage m = wrapper True $ do
  logE <- setupLogin
  logD <- holdDyn "" logE
  nextWidget $ ffor (updated logD) $ \l -> Retractable {
      retractableNext = setupPatternPage m l
    , retractablePrev = Just $ pure $ setupLoginPage m
    }
  pure ()

setupPatternPage :: MonadFrontBase t m => Mnemonic -> Text -> m ()
setupPatternPage m l = wrapper True $ do
  patE <- setupPattern
  patD <- holdDyn "" patE
  let logPassE = fmap (\p -> (l,p)) patE
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo m) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE

askPasswordPage :: MonadFrontBase t m => Text -> m (Event t Password)
askPasswordPage name = wrapper True $ do
  divClass "password-ask-title" $ h4 $ localizedText PPSUnlock
#ifdef ANDROID
  askPattern name
#else
  askPassword
#endif
