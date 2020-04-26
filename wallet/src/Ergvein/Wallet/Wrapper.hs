module Ergvein.Wallet.Wrapper(
    wrapper
  , wrapperSimple
  ) where

import Ergvein.Wallet.Alert.Handler
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad

-- | Common page wrapper. Contains header menu with back button.
wrapper :: (MonadFront t m, LocalizedPrint l) => l -> Maybe (Dynamic t (m ())) -> Bool -> m a -> m a
wrapper titleVal prevWidget centered ma = divClass "base-container" $ do
  headerWidget titleVal prevWidget
  alertHandlerWidget
  if centered then divClass "content-wrapper centered-wrapper" $ divClass "centered-content" $ ma else divClass "content-wrapper" ma


-- | Simplified page wrapper. Contains header with back button only.
wrapperSimple :: MonadFrontBase t m => Bool -> m a -> m a
wrapperSimple centered ma = divClass "base-container" $ do
  headerWidgetOnlyBackBtn
  alertHandlerWidget
  if centered then divClass "content-wrapper centered-wrapper" $ divClass "centered-content" $ ma else divClass "content-wrapper" ma
