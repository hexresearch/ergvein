{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Password(
    passwordPage
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
#ifdef ANDROID
  logPassE <- setupLoginPattern
#else
  logPassE <- setupLoginPassword
#endif
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo mnemonic) logPassE
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
