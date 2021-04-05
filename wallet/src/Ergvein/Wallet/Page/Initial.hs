{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Initial(
    initialPage
  ) where

import Ergvein.Either
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Sepulcas.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Initial
import Ergvein.Wallet.Monad
import Sepulcas.Native
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Page.Settings.Unauth
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Storage.AuthInfo
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Wrapper

import Ergvein.Wallet.Page.PatternKey
import qualified Data.Map.Strict as M

data GoPage = GoSeed | GoRestore | GoSettings

initialPage :: MonadFrontBase t m => Bool -> m ()
initialPage redir = do
  logWrite "Initial page rendering"
  ss <- listStorages
  if null ss then noWalletsPage else hasWalletsPage redir ss
  logWrite "Finished initial page rendering"

noWalletsPage :: MonadFrontBase t m => m ()
noWalletsPage = wrapperSimple True $ divClass "initial-page-options" $ createRestore

createRestore :: MonadFrontBase t m => m ()
createRestore = do
  let items = [(GoSeed, IPSCreate), (GoRestore, IPSRestore), (GoSettings, IPSSettings)]
  goE <- fmap leftmost $ flip traverse items $ \(act, lbl) ->
    fmap (act <$) $ outlineButton lbl
  void $ nextWidget $ ffor goE $ \go -> Retractable {
      retractableNext = case go of
        GoSeed -> mnemonicPage
        GoRestore -> seedRestorePage
        GoSettings -> settingsPageUnauth
    , retractablePrev = Just $ pure $ initialPage False
    }

{-
data GoRestoreMethodPage = GoRestoreMnemonic

selectRestoreMethodPage :: MonadFrontBase t m => m ()
selectRestoreMethodPage = do
  wrapperSimple True $ do
    h4 $ localizedText IPSChooseRestorationMethod
    divClass "initial-options grid1" $ do
      goRestoreMnemonicE  <- fmap (GoRestoreMnemonic  <$) $ outlineButton IPSRestoreFromMnemonic
      void $ nextWidget $ ffor goRestoreMnemonicE $ \page -> Retractable {
          retractableNext = case page of
            GoRestoreMnemonic -> seedRestorePage
        , retractablePrev = Just $ pure selectRestoreMethodPage
        }
-}

hasWalletsPage :: MonadFrontBase t m => Bool -> [WalletName] -> m ()
hasWalletsPage redir ss = do
  buildE <- getPostBuild
  mnameE <- performEvent $ getLastStorage <$ buildE
  void $ nextWidget $ ffor mnameE $ \mname -> Retractable {
      retractableNext = maybe (selectWalletsPage ss) selectNext mname
    , retractablePrev = Just $ pure $ initialPage False
    }
  where
    selectNext = if redir then loadWalletPage else const (selectWalletsPage ss)

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
  buildE <- getPostBuild
  mPlainE <- performEvent $ (loadAuthInfo name "") <$ buildE
  let oldAuthE' = fmapMaybe eitherToMaybe mPlainE
  oldAuthE'' <- fmap switchDyn $ networkHold (pure never) $ ffor mPlainE $ \case
    Right _ -> pure never
    Left _ -> do
      passE <- askPasswordPage name
      mOldAuthE <- performEvent $ loadAuthInfo name <$> passE
      handleDangerMsg mOldAuthE
  let oldAuthE = leftmost [oldAuthE', oldAuthE'']
  mAuthE <- performEvent $ generateMissingPrvKeys <$> oldAuthE
  authE <- handleDangerMsg mAuthE
  when isAndroid $ performEvent_ $ ffor authE $ const $ do
    c <- loadCounter
    saveCounter $ PatternTries $ M.insert name 0 (patterntriesCount c)
  void $ setAuthInfo $ Just <$> authE
