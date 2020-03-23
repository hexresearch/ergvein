module Ergvein.Wallet.Wrapper(
    wrapper
  , wrapperSimple
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad

-- | Common page wrapper. Contains header menu with back button.
wrapper :: (MonadFront t m, LocalizedPrint l) => l -> Maybe (Dynamic t (m ())) -> Bool -> m a -> m a
wrapper titleVal prevWidget centered ma = divClass "base-container" $ do
  menuWidget titleVal prevWidget
  if centered then divClass "content-container vertical-center" ma else divClass "content-container" ma

-- | Simplified page wrapper. Contains header with back button only.
wrapperSimple :: MonadFrontBase t m => Bool -> m a -> m a
wrapperSimple centered ma = divClass "base-container" $ do
  menuWidgetOnlyBackBtn
  if centered then divClass "content-container vertical-center" ma else divClass "content-container" ma
