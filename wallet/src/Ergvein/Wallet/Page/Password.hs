{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Password(
    passwordPage
  , setupLoginPage
  , setupPatternPage
  , askPasswordPage
  , askTextPasswordPage
  ) where

import Ergvein.Crypto.Keys     (Mnemonic)
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Restore
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Password
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.AuthInfo
import Ergvein.Wallet.Wrapper
import Reflex.Localize

passwordPage :: MonadFrontBase t m => WalletSource -> Maybe DerivPrefix -> Mnemonic -> [Currency] -> m ()
passwordPage wt mpath mnemonic curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  rec
    logPassE <- setupLoginPassword btnE
    pathD <- setupDerivPrefix curs mpath
    btnE <- submitSetBtn
  createStorageE <- performEvent $ ffor logPassE $ \(login, pass) -> do
    path <- sample . current $ pathD
    initAuthInfo wt (Just path) mnemonic curs login pass
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE

setupLoginPage :: MonadFrontBase t m => WalletSource -> Maybe DerivPrefix -> Mnemonic -> [Currency] -> m ()
setupLoginPage wt mpath mnemonic curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText LPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText LPSDescr
  rec
    logD <- holdDyn "" =<< setupLogin btnE
    pathD <- setupDerivPrefix curs mpath
    btnE <- submitSetBtn
  void $ nextWidget $ ffor (updated ((,) <$> logD <*> pathD)) $ \(l, path) -> Retractable {
      retractableNext = setupPatternPage wt (Just path) mnemonic l curs
    , retractablePrev = Just $ pure $ setupLoginPage wt (Just path) mnemonic curs
    }

setupPatternPage :: MonadFrontBase t m => WalletSource -> Maybe DerivPrefix -> Mnemonic -> Text -> [Currency] -> m ()
setupPatternPage wt mpath mnemonic l curs = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PatPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PatPSDescr
  patE <- setupPattern
  let logPassE = fmap (\p -> (l,p)) patE
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo wt mpath mnemonic curs) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE

askPasswordPage :: MonadFrontBase t m => Text -> m (Event t Password)
askPasswordPage name = wrapperSimple True $ askPassword name

askTextPasswordPage :: (MonadFrontBase t m, LocalizedPrint l1, LocalizedPrint l2) => l1 -> l2 -> m (Event t Password)
askTextPasswordPage title description = wrapperSimple True $ askTextPassword title description
