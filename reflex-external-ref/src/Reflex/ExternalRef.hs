{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}

-- | External reference with reactivity support. The reference is needed in glue
-- code between reflex and external libs where you cannot sample from dynamics
-- with `MonadSample`.
module Reflex.ExternalRef(
    ExternalRef(..)
  , newExternalRef
  , readExternalRef
  , writeExternalRef
  , modifyExternalRef
  , modifyExternalRefM
  , externalRefBehavior
  , externalRefDynamic
  , externalFromDynamic
  , fmapExternalRef
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.IORef
import GHC.Generics
import Reflex
import Reflex.Dom

-- | Holds value of type `a` and provides ways for notifying FRP network about
-- changes of the variable.
--
-- This abstraction is helpful for storing counters, lists of internal resources
-- and so on. It is designed to be updated from outputs of FRP network, not from
-- outer world.
data ExternalRef t a = ExternalRef {
  -- | Storage of value (do not change this by yourself, use helpers)
  externalRef   :: !(IORef a)
  -- | Event that fires when value is changed
, externalEvent :: !(Event t a)
  -- | Method of updating value of previous event
, externalFire  :: !(a -> IO ())
} deriving (Generic)

instance NFData (ExternalRef t a) where
  rnf ExternalRef{..} =
    externalRef `deepseq`
    externalEvent `seq`
    externalFire `seq`
    ()

-- | Creation of external ref in host monad
newExternalRef :: (MonadIO m, TriggerEvent t m) => a -> m (ExternalRef t a)
newExternalRef a = do
  ref       <- liftIO $ newIORef a
  (e, fire) <- newTriggerEvent
  return $ ExternalRef ref e fire

-- | Read current value of external reference
readExternalRef :: MonadIO m => ExternalRef t a -> m a
readExternalRef ExternalRef {..} = liftIO $ readIORef externalRef

-- | Write new value to external reference and notify FRP network.
-- The function evaluates the value to WNF.
writeExternalRef :: MonadIO m => ExternalRef t a -> a -> m ()
writeExternalRef ExternalRef {..} a = do
  a `seq` liftIO (writeIORef externalRef a)
  _ <- liftIO $ externalFire a
  return ()

-- | Atomically modify an external ref and notify FRP network.
-- The function evaluates the value to WNF.
modifyExternalRef :: MonadIO m => ExternalRef t a -> (a -> (a, b)) -> m b
modifyExternalRef ExternalRef {..} f = do
  (a, b) <- liftIO $ atomicModifyIORef' externalRef $ \a ->
    let (a', b) = f a in (a', (a', b))
  _ <- liftIO $ externalFire a
  return b

-- | Modify (not atomically) an external ref and notify FRP network.
-- The function evaluates the value to WNF.
modifyExternalRefM :: MonadIO m => ExternalRef t a -> (a -> m (a, b)) -> m b
modifyExternalRefM ExternalRef {..} f = do
  a       <- liftIO $ readIORef externalRef
  (a', b) <- f a
  liftIO $ do
    writeIORef externalRef a'
    _ <- externalFire a
    return ()
  return b

-- | Construct a behavior from external reference
externalRefBehavior :: (MonadHold t m, MonadIO m) => ExternalRef t a -> m (Behavior t a)
externalRefBehavior ExternalRef {..} = do
  a <- liftIO $ readIORef externalRef
  hold a externalEvent

-- | Get dynamic that tracks value of the internal ref
externalRefDynamic :: (MonadHold t m, MonadIO m) => ExternalRef t a -> m (Dynamic t a)
externalRefDynamic ExternalRef {..} = do
  a <- liftIO $ readIORef externalRef
  holdDyn a externalEvent

-- | Create external ref that tracks content of dynamic. Editing of the ref
-- has no effect on the original dynamic.
externalFromDynamic :: (MonadHold t m, TriggerEvent t m, PerformEvent t m, Reflex t, MonadIO m, MonadIO (Performable m))
  => Dynamic t a -> m (ExternalRef t a)
externalFromDynamic da = do
  a0 <- sample . current $ da
  r <- newExternalRef a0
  performEvent_ $ fmap (writeExternalRef r) $ updated da
  pure r

-- | Creates external ref as a result of "fmapping" a function to the original ref.
-- ExternalRef t is not a true Functior, since it requres monadic action to "fmap"
-- Editing of the new ref has no effect on the original dynamic.
fmapExternalRef :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (a -> b) -> ExternalRef t a -> m (ExternalRef t b)
fmapExternalRef f ea = do
  v0 <- readExternalRef ea
  r  <- newExternalRef $ f v0
  performEvent_ $ fmap (writeExternalRef r . f) $ externalEvent ea
  pure r
