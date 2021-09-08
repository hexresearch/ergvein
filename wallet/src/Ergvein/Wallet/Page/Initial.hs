{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Initial(
    initialPage
  , OpenLastWallet(..)
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

data GoPage = GoCreate | GoRestore | GoSettings

data OpenLastWallet = OpenLastWalletOn | OpenLastWalletOff

initialPage :: MonadFrontBase t m => OpenLastWallet -> m ()
initialPage openLastWallet = do
  logWrite "Initial page rendering"
  ss <- listStorages
  if null ss then noWalletsPage else hasWalletsPage openLastWallet ss
  logWrite "Finished initial page rendering"

noWalletsPage :: MonadFrontBase t m => m ()
noWalletsPage = wrapperSimple True $ divClass "initial-page-options" createRestore

createRestore :: MonadFrontBase t m => m ()
createRestore = do
  let items = [(GoCreate, IPSCreate), (GoRestore, IPSRestore), (GoSettings, IPSSettings)]
  goE <- fmap leftmost $ for items $ \(act, lbl) ->
    (act <$) <$> outlineButton lbl
  void $ nextWidget $ ffor goE $ \go -> Retractable {
      retractableNext = case go of
        GoCreate -> backupPage
        GoRestore -> simpleSeedRestorePage
        GoSettings -> settingsPageUnauth
    , retractablePrev = Just $ pure $ initialPage OpenLastWalletOff
    }

hasWalletsPage :: MonadFrontBase t m => OpenLastWallet -> [WalletName] -> m ()
hasWalletsPage openLastWallet ss = do
  buildE <- getPostBuild
  mNameE <- performEvent $ getLastStorage <$ buildE
  void $ nextWidget $ ffor mNameE $ \mName -> Retractable {
      retractableNext = maybe (selectWalletsPage ss) selectNext mName
    , retractablePrev = selectPrev =<< mName
    }
  where
    selectNext walletName = case openLastWallet of
      OpenLastWalletOn -> loadWalletPage walletName
      OpenLastWalletOff -> selectWalletsPage ss
    selectPrev _ = case openLastWallet of
      OpenLastWalletOn -> Just $ pure $ initialPage OpenLastWalletOff
      OpenLastWalletOff -> Nothing

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
