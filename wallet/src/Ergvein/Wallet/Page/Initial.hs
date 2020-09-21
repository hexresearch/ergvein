{-# LANGUAGE CPP #-}

module Ergvein.Wallet.Page.Initial(
    initialPage
  ) where

import Control.Monad.IO.Class
import Data.Either (isLeft)
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Initial
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Storage.AuthInfo
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M

data GoPage = GoSeed | GoRestore

data GoRestoreMethodPage = GoRestoreMnemonic | GoRestoreSeed

initialPage :: MonadFrontBase t m => m ()
initialPage = do
  logWrite "Initial page rendering"
  ss <- listStorages
  if null ss then noWalletsPage else hasWalletsPage ss
  logWrite "Finished initial page rendering"

noWalletsPage :: MonadFrontBase t m => m ()
noWalletsPage = wrapperSimple True $ divClass "initial-page-options" $ createRestore

createRestore :: MonadFrontBase t m => m ()
createRestore = do
  newE <- fmap (GoSeed <$) $ outlineButton IPSCreate
  restoreE <- fmap (GoRestore <$) $ outlineButton IPSRestore
  let goE = leftmost [newE, restoreE]
  void $ nextWidget $ ffor goE $ \go -> Retractable {
      retractableNext = case go of
        GoSeed -> mnemonicPage
        GoRestore -> selectRestoreMethodPage
    , retractablePrev = Just $ pure initialPage
    }

selectRestoreMethodPage :: MonadFrontBase t m => m ()
selectRestoreMethodPage = do
  wrapperSimple True $ do
    h4 $ localizedText IPSChooseRestorationMethod
    divClass "initial-options grid1" $ do
      goRestoreMnemonicE <- fmap (GoRestoreMnemonic <$) $ outlineButton IPSRestoreFromMnemonic
      goRestoreSeedE     <- fmap (GoRestoreSeed     <$) $ outlineButton IPSRestoreFromSeed
      let goE = leftmost [
              goRestoreMnemonicE
            , goRestoreSeedE
            ]
      void $ nextWidget $ ffor goE $ \page -> Retractable {
          retractableNext = case page of
            GoRestoreMnemonic -> restoreFromMnemonicPage
            GoRestoreSeed     -> restoreFromSeedPage
        , retractablePrev = Just $ pure selectRestoreMethodPage
        }

hasWalletsPage :: MonadFrontBase t m => [WalletName] -> m ()
hasWalletsPage ss = do
  mname <- getLastStorage
  selectTrougth ss mname

selectTrougth :: MonadFrontBase t m => [WalletName] -> Maybe WalletName -> m ()
selectTrougth ss mname = do
  buildE <- getPostBuild
  case mname of
    Just name -> void $ nextWidget $ ffor buildE $ const $ Retractable {
            retractableNext = loadWalletPage name
          , retractablePrev = Just $ pure $ selectWalletsPage ss
          }
    Nothing -> selectWalletsPage ss

selectWalletsPage :: MonadFrontBase t m => [WalletName] -> m ()
selectWalletsPage ss = wrapperSimple True $ divClass "initial-page-options" $ do
  h4 $ localizedText IPSSelectWallet
  flip traverse_ ss $ \name -> do
    btnE <- outlineButton name
    void $ nextWidget $ ffor btnE $ const $ Retractable {
        retractableNext = loadWalletPage name
      , retractablePrev = Just $ pure $ selectWalletsPage ss
      }
  h4 $ localizedText IPSOtherOptions
  createRestore

loadWalletPage :: MonadFrontBase t m => WalletName -> m ()
loadWalletPage name = do
  passE <- askPasswordPage name
  mOldAuthE <- performEvent $ loadAuthInfo name <$> passE
  oldAuthE <- handleDangerMsg mOldAuthE
  mAuthE <- performEvent $ generateMissingPrvKeys <$> oldAuthE
  authE <- handleDangerMsg mAuthE
#ifdef ANDROID
  performEvent_ $ resetPasswordTimer name <$ authE
#endif
  void $ setAuthInfo $ Just <$> authE

#ifdef ANDROID
resetPasswordTimer :: MonadIO m => WalletName -> m ()
resetPasswordTimer walletName = do
  c <- liftIO $ loadCounter
  liftIO $ saveCounter $ PatternTries $ M.insert walletName 0 (patterntriesCount c)
#endif