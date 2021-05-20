{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Sepulcas.Resize(
    PerformResize
  , adaptive
  , adaptive3
  ) where

import Control.Monad.Fix
import Data.Text (Text)
import Language.Javascript.JSaddle
import Reflex.Dom

-- | Critical width when we should change
-- mobile-desktop widgets between each other
mediumWidth :: Int
mediumWidth = 640

-- | Critical width when we should change from medium
-- to extra small screens.
smallWidth :: Int
smallWidth = 290

-- | Get width of screen
getWidth :: MonadJSM m => m Int
getWidth = liftJSM $ do
  window <- jsg ("window" :: Text)
  fromJSValUnchecked =<< window ! ("innerWidth" :: Text)

type PerformResize t m = (MonadJSM m, DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m), MonadFix m)

-- | Use first for screens with small width and the second
-- for large desktop screen. Note that checks are done only
-- when widget is resized, so use width filling widgets.
adaptive :: forall t m a . PerformResize t m => m a -> m a -> m (Dynamic t a)
adaptive mobileW desktopW = do
  w0 <- getWidth
  workflow $ go w0
  where
    go :: Int -> Workflow t m a
    go w0 = Workflow $ do
      (e, a) <- resizeDetector $ chooseWidget w0
      mnextE <- performEvent $ ffor e $ const $ do
        w1 <- getWidth
        pure $ if rangeOf w0 /= rangeOf w1 then Just w1 else Nothing
      let nextE = go <$> fmapMaybe id mnextE
      pure (a, nextE)

    chooseWidget w = if w <= mediumWidth then mobileW else desktopW
    rangeOf w | w < mediumWidth = 0
              | otherwise = 1

-- | Use first for screens with small width and the second
-- for large desktop screen. Note that checks are done only
-- when widget is resized, so use width filling widgets.
adaptive3 :: forall t m a . PerformResize t m => m a -> m a -> m a -> m (Dynamic t a)
adaptive3 smallW mediumW desktopW = do
  w0 <- getWidth
  workflow $ go w0
  where
    go :: Int -> Workflow t m a
    go w0 = Workflow $ do
      (e, a) <- resizeDetector $ chooseWidget w0
      mnextE <- performEvent $ ffor e $ const $ do
        w1 <- getWidth
        pure $ if rangeOf w0 /= rangeOf w1 then Just w1 else Nothing
      let nextE = go <$> fmapMaybe id mnextE
      pure (a, nextE)

    chooseWidget w
      | w <= smallWidth = smallW
      | w <= mediumWidth = mediumW
      | otherwise = desktopW
    rangeOf w
      | w <= smallWidth = 0
      | w <= mediumWidth = 1
      | otherwise = 2
