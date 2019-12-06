module Ergvein.Wallet.Page.Initial(
    initialPage
  , initialAuthedPage
  ) where

import Ergvein.Wallet.Alert
import Ergvein.Wallet.Alert.Type
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Initial
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Password
import Ergvein.Wallet.Storage.AuthInfo
import Ergvein.Wallet.Wrapper

import Control.Monad.IO.Class
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Password

import Ergvein.Wallet.Storage.Util

data GoPage = GoSeed | GoRestore | Insta Text

initialPage :: MonadFrontBase t m => m ()
initialPage = do
    ss <- listStorages
    if null ss then noWalletsPage else hasWalletsPage ss
  where
    noWalletsPage = wrapper True $ divClass "initial-options grid1" $ do
      newE <- fmap (GoSeed <$) $ outlineButton IPSCreate
      restoreE <- fmap (GoRestore <$) $ outlineButton IPSRestore
      let goE = leftmost [newE, restoreE]
      void $ nextWidget $ ffor goE $ \go -> Retractable {
          retractableNext = case go of
            GoSeed -> mnemonicPage
            GoRestore -> seedRestorePage
        , retractablePrev = Just $ pure initialPage
        }
    hasWalletsPage ss = do
      mname <- getLastStorage
      maybe (selectWalletsPage ss) loadWalletPage mname
    selectWalletsPage ss = wrapper True $ do
       h4 $ localizedText IPSSelectWallet
       flip traverse_ ss $ \name -> do
         btnE <- outlineButton name
         void $ nextWidget $ ffor btnE $ const $ Retractable {
             retractableNext = loadWalletPage name
           , retractablePrev = Just $ pure $ selectWalletsPage ss
           }
    loadWalletPage name = do
      passE <- askPasswordPage
      mauthE <- performEvent $ loadAuthInfo name <$> passE
      authE <- handleDangerMsg mauthE
      void $ setAuthInfo $ Just <$> authE

initialAuthedPage :: MonadFront t m => m ()
initialAuthedPage = wrapper True $ divClass "main-page" $ do
  anon_name <- getWalletName
  h4 $ text $ "Congrats " <> anon_name <> "! You've made it!"
  logoutE <- row . outlineButton $ ("Logout" :: Text)
  void $ setAuthInfo $ Nothing <$ logoutE
