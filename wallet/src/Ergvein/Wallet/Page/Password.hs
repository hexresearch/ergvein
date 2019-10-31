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
import Reflex.Localize

passwordPage :: MonadFront t m => Mnemonic -> m ()
passwordPage _ = wrapper True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  void $ setupPassword

askPasswordPage :: MonadFront t m => m ()
askPasswordPage = wrapper True $ do
  divClass "password-ask-title" $ h4 $ localizedText PPSUnlock
  _ <- askPassword
  pure ()
