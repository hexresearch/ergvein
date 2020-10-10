{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ergvein.Wallet.Util(
    widgetHoldDyn
  , updatedWithInit
  , poke
  , sampleDyn
  , check
  , nothingIf
  , dbgPrintE
  , eventToNextFrame
  , eventToNextFrame'
  , eventToNextFrameN
  , eventToNextFrameN'
  , mergeDyn
  , currencyToCurrencyCode
  , currencyCodeToCurrency
  ) where

import Control.Monad.Except
import Reflex.Dom

import qualified Ergvein.Types.Currency as ETC
import Ergvein.Index.Protocol.Types

-- | Same as 'widgetHold' but for dynamic
widgetHoldDyn :: forall t m a . (Reflex t, Adjustable t m, MonadHold t m) => Dynamic t (m a) -> m (Dynamic t a)
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

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x

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

-- | Make new dynamic that is updated with values from given event and the original dynamic
mergeDyn :: (Reflex t, MonadHold t m) => Dynamic t a -> Event t a -> m (Dynamic t a)
mergeDyn d e = do
  v0 <- sampleDyn d
  holdDyn v0 $ leftmost [e, updated d]

currencyToCurrencyCode :: ETC.Currency -> CurrencyCode
currencyToCurrencyCode c = case c of
  ETC.BTC -> BTC
  ETC.ERGO -> ERGO

currencyCodeToCurrency :: CurrencyCode -> ETC.Currency
currencyCodeToCurrency c = case c of
  BTC -> ETC.BTC
  ERGO -> ETC.ERGO
