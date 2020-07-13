module Ergvein.Wallet.Wrapper(
    wrapper
  , wrapperNavbar
  , wrapperSimple
  ) where

import Ergvein.Wallet.Alert.Handler
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad

-- | Common page wrapper. Contains header menu with back button.
wrapper :: MonadFront t m => Bool -> Dynamic t Text -> Maybe (Dynamic t (m ())) -> m a -> m a
wrapper isCentered titleVal prevWidget ma = divClass "wrapper" $ do
  headerWidget titleVal prevWidget
  a <- if isCentered
    then divClass "centered-container" $ divClass "centered-content container p-1" ma
    else divClass "container p-1" ma
  alertHandlerWidget
  pure a

wrapperNavbar :: MonadFront t m => Bool -> Dynamic t Text -> Maybe (Dynamic t (m ())) -> m a -> m b -> m b
wrapperNavbar isCentered titleVal prevWidget navbar ma = divClass "wrapper" $ do
  headerWidget titleVal prevWidget
  navbar
  a <- if isCentered
    then divClass "centered-container" $ divClass "centered-content container p-1" ma
    else divClass "container p-1" ma
  alertHandlerWidget
  pure a

-- | Simplified page wrapper. Contains header with back button only.
wrapperSimple :: MonadFrontBase t m => Bool -> m a -> m a
wrapperSimple isCentered ma = divClass "wrapper" $ do
  headerWidgetOnlyBackBtn
  a <- if isCentered
    then divClass "centered-container" $ divClass "centered-content container p-1" ma
    else divClass "container p-1" ma
  alertHandlerWidget
  pure a
