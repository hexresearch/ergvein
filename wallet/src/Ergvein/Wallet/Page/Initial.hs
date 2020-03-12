module Ergvein.Wallet.Page.Initial(
    initialPage
  , initialAuthedPage
  ) where

import Data.Text (unpack)

import Ergvein.Wallet.Alert
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Initial
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Password
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.AuthInfo
import Ergvein.Wallet.Widget.GraphPinCode
import Ergvein.Wallet.Wrapper

import Ergvein.Wallet.Storage.Util

data GoPage = GoSeed | GoRestore

initialPage :: MonadFrontBase t m => m ()
initialPage = do
  cameraE <- fmap ("Test" <$) $ outlineButton ("Debug QR scan"::Text)
  _ <- openCamara cameraE
  resButE <- outlineButton ("Get result"::Text)
  resE <- getResultCamara resButE
  resD <- holdDyn "RESULT" resE
  h4 $ dynText resD
  pure ()
{-  ss <- listStorages
  if null ss then noWalletsPage else hasWalletsPage ss
  where
    noWalletsPage = wrapper True $ divClass "initial-options grid1" $ noWallets
    noWallets = do
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
    selectWalletsPage ss = wrapper True $ divClass "initial-options grid1" $ do
      h4 $ localizedText IPSSelectWallet
      flip traverse_ ss $ \name -> do
        btnE <- outlineButton name
        void $ nextWidget $ ffor btnE $ const $ Retractable {
            retractableNext = loadWalletPage name
          , retractablePrev = Just $ pure $ selectWalletsPage ss
          }
      h4 $ localizedText IPSOtherOptions
      noWallets
    loadWalletPage name = do
      passE <- askPasswordPage name
      mauthE <- performEvent $ loadAuthInfo name <$> passE
      authE <- handleDangerMsg mauthE
      void $ setAuthInfo $ Just <$> authE
-}
initialAuthedPage :: MonadFront t m => m ()
initialAuthedPage = wrapper True $ divClass "main-page" $ do
  anon_name <- getWalletName
  h4 $ text $ "Congrats " <> anon_name <> "! You've made it!"
  logoutE <- row . outlineButton $ ("Logout" :: Text)
  void $ setAuthInfo $ Nothing <$ logoutE
