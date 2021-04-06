module Ergvein.Wallet.Wrapper(
    wrapperGeneric
  , wrapper
  , wrapperNavbar
  , wrapperSimple
  ) where

import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Platform
import Sepulcas.Alert.Handler

import qualified Data.Text as T

-- | The most generalized version of the wrapper.
wrapperGeneric :: MonadFront t m => Bool -> Dynamic t Text -> Maybe (Dynamic t (m ())) -> Maybe (m b) -> Text -> m a -> m a
wrapperGeneric isCentered titleVal thisWidget mNavbar classes ma = divClass "wrapper" $ do
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
wrapper isCentered titleVal thisWidget ma = wrapperGeneric isCentered titleVal thisWidget Nothing "" ma

-- | Same as 'wrapper' but with navigation bar.
wrapperNavbar :: MonadFront t m => Bool -> Dynamic t Text -> Maybe (Dynamic t (m ())) -> m b -> m a -> m a
wrapperNavbar isCentered titleVal thisWidget navbar ma = wrapperGeneric isCentered titleVal thisWidget (Just navbar) "" ma

contentContainer :: MonadFront t m => Bool -> Text -> m a -> m a
contentContainer isCentered classes ma = do
  a <- if isCentered
    then divClass "centered-container" $ divClass ("centered-content container p-1" <> padClasses classes) ma
    else divClass ("container p-1" <> padClasses classes) ma
  alertHandlerWidget English
  pure a
  where
    padClasses :: Text -> Text
    padClasses classes = if T.null classes then classes else " " <> classes

-- | Simplified page wrapper. Contains header with back button only.
wrapperSimple :: MonadFrontBase t m => Bool -> m a -> m a
wrapperSimple isCentered ma = divClass "wrapper" $ do
  headerWidgetOnlyBackBtn
  a <- if isCentered
    then divClass "centered-container" $ divClass "centered-content container p-1" ma
    else divClass "container p-1" ma
  alertHandlerWidget English
  pure a
