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
import Ergvein.Wallet.Storage.Data
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Storage.AuthInfo
import Reflex.Localize

passwordPage :: MonadFrontBase t m => Mnemonic -> m ()
passwordPage mnemonic = wrapper True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  --logPassE <- setupLoginPassword
  logPassE <- setupLoginPattern
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo mnemonic) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE

askPasswordPage :: MonadFrontBase t m => m (Event t Password)
askPasswordPage = wrapper True $ do
  divClass "password-ask-title" $ h4 $ localizedText PPSUnlock
  --askPassword
  askPattern
