module Ergvein.Wallet.Wrapper(
    WrapperAlignment(..)
  , WrapperPadding(..)
  , wrapper
  , wrapperSimple
  , contentWrapper
  ) where

import Ergvein.Wallet.Alert.Handler
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad

data WrapperAlignment = WrapperAlignmentNone | WrapperAlignmentVertical | WrapperAlignmentHorizontal | WrapperAlignmentCenter

data WrapperPadding = WrapperPadding | WrapperNoPadding

-- | Common page wrapper. Contains header menu with back button.
wrapper :: (MonadFront t m, LocalizedPrint l) => l -> Maybe (Dynamic t (m ())) -> WrapperAlignment -> m a -> m a
wrapper titleVal prevWidget alignment ma = divClass "base-container" $ do
  headerWidget titleVal prevWidget
  a <- contentWrapper alignment WrapperPadding ma
  alertHandlerWidget
  pure a

-- | Simplified page wrapper. Contains header with back button only.
wrapperSimple :: MonadFrontBase t m => WrapperAlignment -> m a -> m a
wrapperSimple alignment ma = divClass "base-container" $ do
  headerWidgetOnlyBackBtn
  a <- contentWrapper alignment WrapperPadding ma
  alertHandlerWidget
  pure a

contentWrapper :: DomBuilder t m => WrapperAlignment -> WrapperPadding -> m a -> m a
contentWrapper WrapperAlignmentNone WrapperPadding = divClass "content-wrapper pad-1"
contentWrapper WrapperAlignmentNone WrapperNoPadding = divClass "content-wrapper"
contentWrapper WrapperAlignmentVertical WrapperPadding = divClass "content-wrapper-centered pad-1" . divClass "vertically-centered-content"
contentWrapper WrapperAlignmentVertical WrapperNoPadding = divClass "content-wrapper-centered" . divClass "vertically-centered-content"
contentWrapper WrapperAlignmentHorizontal WrapperPadding = divClass "content-wrapper-centered pad-1" . divClass "horizontally-centered-content"
contentWrapper WrapperAlignmentHorizontal WrapperNoPadding = divClass "content-wrapper-centered" . divClass "horizontally-centered-content"
contentWrapper WrapperAlignmentCenter WrapperPadding = divClass "content-wrapper-centered pad-1" . divClass "centered-content"
contentWrapper WrapperAlignmentCenter WrapperNoPadding = divClass "content-wrapper-centered" . divClass "centered-content"
