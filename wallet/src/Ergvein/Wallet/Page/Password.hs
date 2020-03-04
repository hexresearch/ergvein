{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Password(
    passwordPage
  , setupLoginPage
  , setupPatternPage
  , askPasswordPage
  ) where

import Control.Monad.IO.Class
import Data.Map as Map

import Ergvein.Crypto.Keys     (Mnemonic)
import Ergvein.Types.Currency
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
passwordPage mnemonic curs = wrapper True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSDescr
  logPassE <- setupLoginPassword
  s <- getSettings
  updateSettings $ ffor logPassE $ \(l,_) -> s {settingsActiveCurrencies = acSet l s}
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo mnemonic) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE
  where
    acSet l s = ActiveCurrencies $ Map.insert l curs $ activeCurrenciesMap $ settingsActiveCurrencies s

setupLoginPage :: MonadFrontBase t m => Mnemonic -> [Currency] -> m ()
setupLoginPage m ac = wrapper True $ do
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
setupPatternPage m l curs = wrapper True $ do
  divClass "password-setup-title" $ h4 $ localizedText PatPSTitle
  divClass "password-setup-descr" $ h5 $ localizedText PatPSDescr
  --buildE <- getPostBuild
  patE <- setupPattern
  let logPassE = fmap (\p -> (l,p)) patE
  s <- getSettings
  updateSettings $ ffor logPassE $ \_ -> s {settingsActiveCurrencies = acSet l s}
  --createStorageE <- delay 0.1 =<< (performEvent $ fmap (uncurry $ initAuthInfo m) logPassE)
  createStorageE <- performEvent $ fmap (uncurry $ initAuthInfo m) logPassE
  authInfoE <- handleDangerMsg createStorageE
  void $ setAuthInfo $ Just <$> authInfoE
  --goE <- setAuthInfo $ Just <$> authInfoE
  --nextWidget $ ffor goE $ \_ -> Retractable {
  --    retractableNext = void (accountDiscovery >> retractStack balancesPage)
  --  , retractablePrev = Just $ pure $ setupLoginPage m curs
  --  }
  --pure ()
  where
    acSet l s = ActiveCurrencies $ Map.insert l curs $ activeCurrenciesMap $ settingsActiveCurrencies s

askPasswordPage :: MonadFrontBase t m => Text -> m (Event t Password)
askPasswordPage name = wrapper True $ do
  divClass "password-ask-title" $ h4 $ localizedText PPSUnlock
#ifdef ANDROID
  askPattern name
#else
  askPassword
#endif
