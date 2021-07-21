{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Initial(
    initialPage
  ) where

import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Traversable (for)

import Ergvein.Either
import Ergvein.Types.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Page.Settings.Unauth
import Ergvein.Wallet.Wrapper
import Sepulcas.Alert
import Sepulcas.Elements

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

data GoPage = GoCreate Mnemonic | GoRestore | GoSettings

initialPage :: MonadFrontBase t m => Bool -> m ()
initialPage redir = do
  logWrite "Initial page rendering"
  ss <- listStorages
  if null ss then noWalletsPage else hasWalletsPage redir ss
  logWrite "Finished initial page rendering"

noWalletsPage :: MonadFrontBase t m => m ()
noWalletsPage = wrapperSimple True $ divClass "initial-page-options" createRestore

createRestore :: MonadFrontBase t m => m ()
createRestore = do
  createE <- outlineButton IPSCreate
  eMnemonicE <- performFork $ ffor createE $ const $ do
    entropy <- liftIO getEntropy
    pure $ first T.pack $ toMnemonic entropy
  mnemonicE <- (fmap . fmap) GoCreate (handleDangerMsg eMnemonicE)
  restoreE <- (GoRestore <$) <$> outlineButton IPSRestore
  settingsE <- (GoSettings <$) <$> outlineButton IPSSettings
  let goE = leftmost [mnemonicE, restoreE, settingsE]
  void $ nextWidget $ ffor goE $ \go -> Retractable {
      retractableNext = case go of
        GoCreate mnemonic -> setLoginPasswordPage WalletGenerated mnemonic
        GoRestore -> simpleSeedRestorePage
        GoSettings -> settingsPageUnauth
    , retractablePrev = Just $ pure $ initialPage False
    }

hasWalletsPage :: MonadFrontBase t m => Bool -> [WalletName] -> m ()
hasWalletsPage redir ss = do
  buildE <- getPostBuild
  mnameE <- performEvent $ getLastStorage <$ buildE
  void $ nextWidget $ ffor mnameE $ \mname -> Retractable {
      retractableNext = maybe (selectWalletsPage ss) selectNext mname
    , retractablePrev = Just $ pure $ selectWalletsPage ss
    }
  where
    selectNext = if redir then loadWalletPage else const (selectWalletsPage ss)

selectWalletsPage :: MonadFrontBase t m => [WalletName] -> m ()
selectWalletsPage ss = wrapperSimple True $ divClass "initial-page-options" $ do
  h4 $ localizedText IPSSelectWallet
  for_ ss $ \name -> do
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
  mPlainE <- performEvent $ loadWalletInfo name "" <$ buildE
  let oldAuthE' = fmapMaybe eitherToMaybe mPlainE
  oldAuthE'' <- fmap switchDyn $ networkHold (pure never) $ ffor mPlainE $ \case
    Right _ -> pure never
    Left _ -> do
      passE <- askPasswordPage name
      mOldAuthE <- performEvent $ loadWalletInfo name <$> passE
      handleDangerMsg mOldAuthE
  let oldAuthE = leftmost [oldAuthE', oldAuthE'']
  mAuthE <- performEvent $ generateMissingPrvKeys <$> oldAuthE
  authE <- handleDangerMsg mAuthE
  when isAndroid $ performEvent_ $ ffor authE $ const $ do
    c <- loadCounter
    saveCounter $ PatternTries $ M.insert name 0 (patterntriesCount c)
  void $ setWalletInfo $ Just <$> authE
