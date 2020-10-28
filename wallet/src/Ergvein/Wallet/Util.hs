{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ergvein.Wallet.Util(
    widgetHoldDyn
  , widgetHoldE
  , widgetHoldDynE
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
  , splitEither
  , splitFilter
  , switchDyn2
  ) where

import Control.Monad.Except
import Reflex.Dom

import Ergvein.Index.Protocol.Types
import Ergvein.Wallet.Platform

import qualified Ergvein.Types.Currency as ETC

-- | Same as 'widgetHold' but for dynamic
widgetHoldDyn :: forall t m a . (Reflex t, Adjustable t m, MonadHold t m) => Dynamic t (m a) -> m (Dynamic t a)
widgetHoldDyn maD = do
  ma <- sample . current $ maD
  widgetHold ma $ updated maD
{-# INLINABLE widgetHoldDyn #-}

-- | Same as `widgetHold` but tailored for widgets returning events
widgetHoldE :: forall t m a . (Reflex t, Adjustable t m, MonadHold t m) => m (Event t a) -> Event t (m (Event t a)) -> m (Event t a)
widgetHoldE m0 ma = fmap switchDyn $ widgetHold m0 ma
{-# INLINABLE widgetHoldE #-}

-- | Same as `widgetHoldDyn` but tailored for widgets returning events
widgetHoldDynE :: forall t m a . (Reflex t, Adjustable t m, MonadHold t m) => Dynamic t (m (Event t a)) -> m (Event t a)
widgetHoldDynE = fmap switchDyn . widgetHoldDyn
{-# INLINABLE widgetHoldDynE #-}

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
  ETC.BTC -> if isTestnet then TBTC else BTC
  ETC.ERGO -> if isTestnet then TERGO else ERGO

currencyCodeToCurrency :: CurrencyCode -> ETC.Currency
currencyCodeToCurrency c = case c of
  BTC -> ETC.BTC
  TBTC -> ETC.BTC
  ERGO -> ETC.ERGO
  TERGO -> ETC.ERGO
  _ -> error "Currency code not implemented"

splitEither :: Reflex t => Event t (Either a b) -> (Event t a, Event t b)
splitEither e = (ae, be)
  where
    ae = fmapMaybe (either Just (const Nothing)) e
    be = fmapMaybe (either (const Nothing) Just) e

switchDyn2 :: Reflex t => Dynamic t (Event t a, Event t b) -> (Event t a, Event t b)
switchDyn2 = (\(a,b) -> (switchDyn a, switchDyn b)) . splitDynPure

splitFilter :: Reflex t => (a -> Bool) -> Event t a -> (Event t a, Event t a)
splitFilter f e = (ffilter f e, ffilter (not . f) e)
