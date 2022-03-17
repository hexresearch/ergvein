module Ergvein.Wallet.Page.Initial(
    initialPage
  , OpenLastWallet(..)
  ) where

import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Traversable (for)

import Ergvein.Either
import Ergvein.Types.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Page.Settings.Unauth
import Ergvein.Wallet.Password
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

  tE <- outlineButton ("Set" :: Text)
  tE' <- performEvent $ ffor tE $ const $ androidSetScreenFlag
  cE <- outlineButton ("Clear" :: Text)
  cE' <- performEvent $ ffor cE $ const $ androidClearScreenFlag

  tglD <- holdDyn False $ leftmost [True <$ tE', False <$ cE']
  networkHoldDyn $ ffor tglD $ \case
    False -> el "div" $ text "Clear"
    True -> el "div" $ text "Set"


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
  -- We could use `wrapperSimple True` here, but to draw the pin code widget to full screen, we need to use this
  wrapperSimpleGeneric headerWidgetOnlyBackBtn "password-widget-container" False $ do
    buildE <- getPostBuild
    ePlainE <- performEvent $ loadWalletInfo name "" <$ buildE
    let oldAuthE' = fmapMaybe eitherToMaybe ePlainE
    oldAuthE'' <- fmap switchDyn $ networkHold (pure never) $ ffor ePlainE $ \case
      Right _ -> pure never
      Left _ -> mdo
        passE <- askPasswordWidget name True invalidPassE
        eOldAuthE <- performEvent $ loadWalletInfo name <$> passE
        let invalidPassE = void $ ffilter isLeft eOldAuthE -- We need this event to clear the PIN input after a failed attempt
        handleDangerMsg eOldAuthE
    let oldAuthE = leftmost [oldAuthE', oldAuthE'']
    mAuthE <- performEvent $ generateMissingPrvKeys <$> oldAuthE
    authE <- handleDangerMsg mAuthE
    when isAndroid $ performEvent_ $ ffor authE $ const $ do
      c <- loadCounter
      saveCounter $ PasswordTries $ M.insert name 0 (passwordTriesCount c)
    void $ setWalletInfo $ Just <$> authE
