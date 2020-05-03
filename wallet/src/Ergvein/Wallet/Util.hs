module Ergvein.Wallet.Util(
    widgetHoldDyn
  , updatedWithInit
  , poke
  , sampleDyn
  , check
  , dbgPrintE
  , eventToNextFrame
  , eventToNextFrame'
  , eventToNextFrameN
  , eventToNextFrameN'
  ) where

import Control.Monad.Except
import Reflex.Dom

import Ergvein.Wallet.Monad.Base

-- | Same as 'widgetHold' but for dynamic
widgetHoldDyn :: forall t m a . (DomBuilder t m, MonadHold t m) => Dynamic t (m a) -> m (Dynamic t a)
widgetHoldDyn maD = do
  ma <- sample . current $ maD
  widgetHold ma $ updated maD

-- | Same as 'updated', but fires init value with 'getPostBuild'
updatedWithInit :: PostBuild t m => Dynamic t a -> m (Event t a)
updatedWithInit da = do
  buildE <- getPostBuild
  pure $ leftmost [updated da, current da `tag` buildE]

-- | Fliped version of `pushAlways`
poke :: Reflex t => Event t a -> (a -> PushM t b) -> Event t b
poke = flip pushAlways

-- | Sample dynamic instead of `Behavior`
sampleDyn :: (MonadSample t m, Reflex t) => Dynamic t a -> m a
sampleDyn = sample . current

-- | Helper to throw error when predicate is not 'True'
check :: MonadError a m => a -> Bool -> m ()
check a False = throwError a
check _ True = pure ()

dbgPrintE :: (MonadWidget t m, Show a) => Event t a -> m ()
dbgPrintE = performEvent_ . fmap (liftIO . print)

eventToNextFrame :: MonadWidget t m => Event t a -> m (Event t a)
eventToNextFrame = performEvent . (fmap (liftIO . pure. id))

eventToNextFrame' :: MonadWidget t m => m (Event t a) -> m (Event t a)
eventToNextFrame' evtM = do
  evt <- evtM
  eventToNextFrame evt

eventToNextFrameN :: MonadWidget t m => Int -> Event t a -> m (Event t a)
eventToNextFrameN n evt
  | n < 1     = do performEvent $ (fmap (liftIO . pure. id)) evt
  | otherwise = do evtN <- performEvent $ (fmap (liftIO . pure. id)) evt
                   eventToNextFrameN (n - 1) evtN

eventToNextFrameN' :: MonadWidget t m => Int -> m (Event t a) -> m (Event t a)
eventToNextFrameN' n evtM = do
  evt <- evtM
  eventToNextFrameN n evt
