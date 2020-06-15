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
wrapper :: (MonadFront t m, LocalizedPrint l) => l -> Maybe (Dynamic t (m ())) -> m a -> m a
wrapper titleVal prevWidget ma = divClass "wrapper" $ do
  headerWidget titleVal prevWidget
  a <- ma
  alertHandlerWidget
  pure a

-- | Simplified page wrapper. Contains header with back button only.
wrapperSimple :: MonadFrontBase t m =>  m a -> m a
wrapperSimple ma = divClass "wrapper" $ do
  headerWidgetOnlyBackBtn
  a <- ma
  alertHandlerWidget
  pure a
