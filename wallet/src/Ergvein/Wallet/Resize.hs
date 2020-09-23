{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Ergvein.Wallet.Resize(
    adaptive
  , adaptive3
  ) where

import Ergvein.Wallet.Monad
import Language.Javascript.JSaddle

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
        pure $ if rangeOf w0 /= rangeOf w1 then Just w1 else Nothing
      let nextE = go <$> fmapMaybe id mnextE
      pure (a, nextE)

    chooseWidget w = if w <= mediumWidth then mobileW else desktopW
    rangeOf w | w < mediumWidth = 0
              | otherwise = 1

-- | Use first for screens with small width and the second
-- for large desktop screen. Note that checks are done only
-- when widget is resized, so use width filling widgets.
adaptive3 :: forall t m a . MonadBaseConstr t m => m a -> m a -> m a -> m (Dynamic t a)
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
