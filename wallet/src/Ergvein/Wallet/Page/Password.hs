{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Password(
    passwordPage
  , setupLoginPage
  , setupPatternPage
  , askPasswordPage
  , askTextPasswordPage
  ) where

import Control.Monad.IO.Class

import Ergvein.Crypto.Keys     (Seed)
import Ergvein.Types.Currency
import Ergvein.Types.Restore
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

passwordPage :: MonadFrontBase t m => WalletSource -> Seed -> [Currency] -> m ()
passwordPage wt seed curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  logPassE <- setupLoginPassword
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo wt seed curs) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE

setupLoginPage :: MonadFrontBase t m => WalletSource -> Seed -> [Currency] -> m ()
setupLoginPage wt seed ac = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText LPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText LPSDescr
  logE <- setupLogin
  logD <- holdDyn "" logE
  nextWidget $ ffor (updated logD) $ \l -> Retractable {
      retractableNext = setupPatternPage wt seed l ac
    , retractablePrev = Just $ pure $ setupLoginPage wt seed ac
    }
  pure ()

setupPatternPage :: MonadFrontBase t m => WalletSource -> Seed -> Text -> [Currency] -> m ()
setupPatternPage wt seed l curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PatPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PatPSDescr
  patE <- setupPattern
  let logPassE = fmap (\p -> (l,p)) patE
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo wt seed curs) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE

askPasswordPage :: MonadFrontBase t m => Text -> m (Event t Password)
askPasswordPage name = wrapperSimple True $ askPassword name

askTextPasswordPage :: (MonadFrontBase t m, LocalizedPrint l1, LocalizedPrint l2) => l1 -> l2 -> m (Event t Password)
askTextPasswordPage title description = wrapperSimple True $ askTextPassword title description
