module Ergvein.Wallet.Resize(
    adaptive
  ) where

import Ergvein.Wallet.Monad
import Language.Javascript.JSaddle
import Reflex.Network

-- | Critical width when we should change
-- mobile-desktop widgets between each other
mobileWidth :: Int
mobileWidth = 650

-- | Get width of screen
getWidth :: MonadJSM m => m Int
getWidth = liftJSM $ do
  window <- jsg ("window" :: Text)
  fromJSValUnchecked =<< window ! ("innerWidth" :: Text)

-- | Use first for screens with small width and the second
-- for large desktop screen. Note that checks are done only
-- when widget is resized, so use width filling widgets.
adaptive :: forall t m a . MonadBaseConstr t m => m a -> m a -> m (Dynamic t a)
adaptive mobileW desktopW = do
  w0 <- getWidth
  workflow $ go w0
  where
    go :: Int -> Workflow t m a
    go w0 = Workflow $ do
      (e, a) <- resizeDetector $ chooseWidget w0
      mnextE <- performEvent $ ffor e $ const $ do
        w1 <- getWidth
        pure $ if widthChanged w0 w1 then Just w1 else Nothing
      let nextE = go <$> fmapMaybe id mnextE
      pure (a, nextE)

    chooseWidget w = if w <= mobileWidth then mobileW else desktopW
    widthChanged w0 w1 =
         (w0 <= mobileWidth && w1 > mobileWidth)
      || (w0 > mobileWidth  && w1 <= mobileWidth)
