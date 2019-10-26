module Ergvein.Wallet.Page.Password(
    passwordPage
  , askPasswordPage
  , Mnemonic
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Password
import Ergvein.Wallet.Wrapper

import Ergvein.Wallet.Language
import Reflex.Localize

type Mnemonic = Text

data PasswordPageStrings = PPSTitle | PPSDescr | PPSUnlock

instance LocalizedPrint PasswordPageStrings where
  localizedShow l v = case l of
    English -> case v of
      PPSTitle  -> "Setup encryption password for your wallet"
      PPSDescr  -> "The password is used every time you perform an operation with your money. Leave the fields empty to set no password for your wallet."
      PPSUnlock -> "Unlock your private keys with password"
    Russian -> case v of
      PPSTitle  -> "Установите пароль шифрования для кошелька"
      PPSDescr  -> "Этот пароль используется для каждой операции с вашими деньгами. Можете оставить поле пустым, если хотите (не рекомендуется)"
      PPSUnlock -> "Введите пароль для расшифровки приватных ключей"

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
