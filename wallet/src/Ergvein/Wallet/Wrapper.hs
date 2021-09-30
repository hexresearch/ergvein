module Ergvein.Wallet.Wrapper(
    wrapperGeneric
  , wrapper
  , wrapperNavbar
  , wrapperSimpleGeneric
  , wrapperSimple
  , wrapperSimpleLogout
  , wrapperPasswordModal
  ) where

import {-# SOURCE #-} Ergvein.Wallet.Password
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Sepulcas.Alert.Handler

import qualified Data.Text as T

-- | The most generalized version of the wrapper.
wrapperGeneric :: MonadFront t m => Bool -> Dynamic t Text -> Maybe (Dynamic t (m ())) -> Maybe (m b) -> Text -> m a -> m a
wrapperGeneric isCentered titleVal thisWidget mNavbar classes ma = do
  askPasswordModal
  divClass "wrapper" $ do
    if isAndroid
      then headerWidgetAndroid titleVal thisWidget
      else headerWidgetDesktop titleVal thisWidget
    case mNavbar of
      Nothing -> do
        contentContainer isCentered classes ma
      Just navbar -> do
        void navbar
        contentContainer isCentered classes ma

-- | Common page wrapper. Contains header menu with back button.
wrapper :: MonadFront t m => Bool -> Dynamic t Text -> Maybe (Dynamic t (m ())) -> m a -> m a
wrapper isCentered titleVal thisWidget = wrapperGeneric isCentered titleVal thisWidget Nothing ""

-- | Same as 'wrapper' but with navigation bar.
wrapperNavbar :: MonadFront t m => Bool -> Dynamic t Text -> Maybe (Dynamic t (m ())) -> m b -> m a -> m a
wrapperNavbar isCentered titleVal thisWidget navbar = wrapperGeneric isCentered titleVal thisWidget (Just navbar) ""

-- Prepends space to text if it is not empty.
padClasses :: Text -> Text
padClasses c = if T.null c then c else " " <> c

contentContainer :: MonadFrontBase t m => Bool -> Text -> m a -> m a
contentContainer isCentered classes ma = do
  a <- if isCentered
    then divClass "centered-container flex-grow" $ divClass ("centered-content container py-1 px-2" <> padClasses classes) ma
    else divClass ("container flex-grow py-1 px-2" <> padClasses classes) ma
  alertHandlerWidget English
  pure a

wrapperSimpleGeneric :: MonadFrontBase t m => m () -> Text -> Bool -> m a -> m a
wrapperSimpleGeneric header classes isCentered ma = divClass "wrapper" $ do
  header
  a <- if isCentered
    then divClass "centered-container flex-grow" $ divClass ("centered-content container py-1 px-2" <> padClasses classes) ma
    else divClass ("container flex-grow py-1 px-2" <> padClasses classes) ma
  alertHandlerWidget English
  pure a

-- | Simplified page wrapper. Contains header with back button only.
wrapperSimple :: MonadFrontBase t m => Bool -> m a -> m a
wrapperSimple = wrapperSimpleGeneric headerWidgetOnlyBackBtn ""

-- | Same as 'wrapperSimple' but "back" buttons performs logout and redirects to the wallet selection page
wrapperSimpleLogout :: MonadFront t m => Bool -> m a -> m a
wrapperSimpleLogout = wrapperSimpleGeneric headerWidgetOnlyLogoutBtn ""

-- | Wrapper for password modal page.
wrapperPasswordModal :: MonadFront t m => Dynamic t Text -> Text -> m a -> m (Event t (), a)
wrapperPasswordModal titleVal classes ma = divClass "wrapper" $ do
  closeE <- if isAndroid
      then headerWidgetAndroidPasswordModal titleVal
      else headerWidgetDesktopPasswordModal titleVal
  a <- contentContainer False classes ma
  pure (closeE, a)
