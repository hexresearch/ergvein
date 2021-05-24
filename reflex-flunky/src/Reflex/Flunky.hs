module Reflex.Flunky(
    networkHold_
  , networkHoldDyn
  , networkHoldE
  , networkHoldDynE
  , updatedWithInit
  , poke
  , sampleDyn
  , holdJust
  , nothingIf
  , dbgPrintE
  , eventToNextFrame
  , eventToNextFrame'
  , eventToNextFrameN
  , eventToNextFrameN'
  , mergeDyn
  , splitEither
  , splitFilter
  , switchDyn2
  , zipDyn3
  , mkChunks
  , EventTrigger(..)
  , newTriggerEvent'
  , triggerPair
  ) where

import Control.Monad.IO.Class
import Data.Functor (void)
import Reflex
import Reflex.Network

networkHold_ :: forall t m a . (Reflex t, Adjustable t m, MonadHold t m) => m a -> Event t (m a) -> m ()
networkHold_ a0 = void . networkHold a0

-- | Same as 'widgetHold' but for dynamic
networkHoldDyn :: forall t m a . (Reflex t, Adjustable t m, MonadHold t m) => Dynamic t (m a) -> m (Dynamic t a)
networkHoldDyn maD = do
  ma <- sample . current $ maD
  networkHold ma $ updated maD
{-# INLINABLE networkHoldDyn #-}

-- | Same as `networkHold` but tailored for widgets returning events
networkHoldE :: forall t m a . (Reflex t, Adjustable t m, MonadHold t m) => m (Event t a) -> Event t (m (Event t a)) -> m (Event t a)
networkHoldE m0 ma = fmap switchDyn $ networkHold m0 ma
{-# INLINABLE networkHoldE #-}

-- | Same as `networkHoldDyn` but tailored for widgets returning events
networkHoldDynE :: forall t m a . (Reflex t, Adjustable t m, MonadHold t m) => Dynamic t (m (Event t a)) -> m (Event t a)
networkHoldDynE = fmap switchDyn . networkHoldDyn
{-# INLINABLE networkHoldDynE #-}

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

-- | Convert event to dynamic that nothing on startup and just when value arrives
holdJust :: (MonadHold t m, Reflex t) => Event t a -> m (Dynamic t (Maybe a))
holdJust = holdDyn Nothing . fmap Just

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x

dbgPrintE :: (PerformEvent t m, Show a, MonadIO (Performable m)) => Event t a -> m ()
dbgPrintE = performEvent_ . fmap (liftIO . print)

eventToNextFrame :: (PerformEvent t m, MonadIO (Performable m)) => Event t a -> m (Event t a)
eventToNextFrame = performEvent . (fmap (liftIO . pure . id))

eventToNextFrame' :: (PerformEvent t m, MonadIO (Performable m)) => m (Event t a) -> m (Event t a)
eventToNextFrame' evtM = do
  evt <- evtM
  eventToNextFrame evt

eventToNextFrameN :: (PerformEvent t m, MonadIO (Performable m)) => Int -> Event t a -> m (Event t a)
eventToNextFrameN n evt
  | n < 1     = do performEvent $ (fmap (liftIO . pure. id)) evt
  | otherwise = do evtN <- performEvent $ (fmap (liftIO . pure. id)) evt
                   eventToNextFrameN (n - 1) evtN

eventToNextFrameN' :: (PerformEvent t m, MonadIO (Performable m)) => Int -> m (Event t a) -> m (Event t a)
eventToNextFrameN' n evtM = do
  evt <- evtM
  eventToNextFrameN n evt

-- | Make new dynamic that is updated with values from given event and the original dynamic
mergeDyn :: (Reflex t, MonadHold t m) => Dynamic t a -> Event t a -> m (Dynamic t a)
mergeDyn d e = do
  v0 <- sampleDyn d
  holdDyn v0 $ leftmost [e, updated d]

splitEither :: Reflex t => Event t (Either a b) -> (Event t a, Event t b)
splitEither e = (ae, be)
  where
    ae = fmapMaybe (either Just (const Nothing)) e
    be = fmapMaybe (either (const Nothing) Just) e

switchDyn2 :: Reflex t => Dynamic t (Event t a, Event t b) -> (Event t a, Event t b)
switchDyn2 = (\(a,b) -> (switchDyn a, switchDyn b)) . splitDynPure

zipDyn3 :: Reflex t => Dynamic t a -> Dynamic t b -> Dynamic t c -> Dynamic t (a, b, c)
zipDyn3 aD bD cD = let abD = zipDyn aD bD
  in zipDynWith (\(a, b) c -> (a, b, c)) abD cD

splitFilter :: Reflex t => (a -> Bool) -> Event t a -> (Event t a, Event t a)
splitFilter f e = (ffilter f e, ffilter (not . f) e)

-- / Make chunks of length n
mkChunks :: Int -> [a] -> [[a]]
mkChunks n vals = mkChunks' [] vals
  where
     mkChunks' acc xs = case xs of
       [] -> acc
       _ -> let (a,b) = splitAt n xs in mkChunks' (acc ++ [a]) b

-- | Helper to store strict pair of event and fire returned from `newTriggerEvent`
data EventTrigger t a = EventTrigger {
  triggerEvent :: !(Event t a)
, triggerFire :: !(a -> IO ())
}

-- | Helper to wrap result of `newTriggerEvent` into strict tuple
newTriggerEvent' :: TriggerEvent t m => m (EventTrigger t a)
newTriggerEvent' = fmap (uncurry EventTrigger) newTriggerEvent

-- | Convert strict pair into tuple back
triggerPair :: EventTrigger t a -> (Event t a, a -> IO ())
triggerPair (EventTrigger e f) = (e, f)
