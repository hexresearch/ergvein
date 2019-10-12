module Reflex.Dom.Retractable.Trans.Internal where

import Control.Monad.Reader
import Control.Monad.State.Strict
import GHC.Generics
import Language.Javascript.JSaddle.Types
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable.Class

type RetractableT t m = Retractable t (RetractT t m)

-- | Internal state of retractable widget
data RetractEnv t m = RetractEnv
  { renvNextFire     :: !(RetractableT t m -> IO ())
  , renvNextEvent    :: !(Event t (RetractableT t m))
  , renvRetractFire  :: !(IO ())
  , renvRetractEvent :: !(Event t ())
  , renvWipeFire     :: !(Maybe Int -> IO ())
  , renvWipeEvent    :: !(Event t (Maybe Int))
  , renvStack        :: !(Dynamic t [RetractableT t m])
  } deriving (Generic)

-- | Allocate new environment for `RetracT`.
newRetractEnv :: (Reflex t, TriggerEvent t m) => m (RetractEnv t m)
newRetractEnv = do
  (nextE, nextFire) <- newTriggerEvent
  (retrE, retrFire) <- newTriggerEvent
  (wipeE, wipeFire) <- newTriggerEvent
  pure RetractEnv {
      renvNextFire     = nextFire
    , renvNextEvent    = nextE
    , renvRetractFire  = retrFire ()
    , renvRetractEvent = retrE
    , renvWipeFire     = wipeFire
    , renvWipeEvent    = wipeE
    , renvStack        = pure []
    }

-- | Plug-in implementation of `MonadRetract`.
newtype RetractT t m a = RetractT { unRetractT :: ReaderT (RetractEnv t m) m a }
  deriving (Functor, Applicative, Monad, Generic, MonadFix)

deriving instance PostBuild t m => PostBuild t (RetractT t m)
deriving instance NotReady t m => NotReady t (RetractT t m)
deriving instance PerformEvent t m => PerformEvent t (RetractT t m)
deriving instance TriggerEvent t m => TriggerEvent t (RetractT t m)
deriving instance MonadHold t m => MonadHold t (RetractT t m)
deriving instance MonadSample t m => MonadSample t (RetractT t m)
deriving instance DomBuilder t m => DomBuilder t (RetractT t m)
deriving instance MonadIO m => MonadIO (RetractT t m)
deriving instance MonadJSM m => MonadJSM (RetractT t m)
deriving instance (Group q, Additive q, Query q, Eq q, MonadQuery t q m, Monad m) => MonadQuery t q (RetractT t m)
deriving instance (Monoid w, DynamicWriter t w m) => DynamicWriter t w (RetractT t m)
deriving instance (Monoid w, MonadBehaviorWriter t w m) => MonadBehaviorWriter t w (RetractT t m)
deriving instance (Semigroup w, EventWriter t w m) => EventWriter t w (RetractT t m)
deriving instance (Requester t m) => Requester t (RetractT t m)

instance MonadTrans (RetractT t) where
  lift = RetractT . lift
  {-# INLINABLE lift #-}

instance MonadReader e m => MonadReader e (RetractT t m) where
  ask = lift ask
  {-# INLINABLE ask #-}
  local f (RetractT ma) = RetractT $ do
    r <- ask
    lift $ local f $ runReaderT ma r
  {-# INLINABLE local #-}

instance MonadState s m => MonadState s (RetractT t m) where
  get = lift get
  {-# INLINABLE get #-}
  put = lift . put
  {-# INLINABLE put #-}

instance Adjustable t m => Adjustable t (RetractT t m) where
  runWithReplace a0 a' = do
    r <- RetractT ask
    lift $ runWithReplace (runRetractT a0 r) $ fmap (`runRetractT` r) a'
  {-# INLINABLE runWithReplace #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    r <- RetractT ask
    lift $ traverseIntMapWithKeyWithAdjust (\k v -> runRetractT (f k v) r) dm0 dm'
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    r <- RetractT ask
    lift $ traverseDMapWithKeyWithAdjust (\k v -> runRetractT (f k v) r) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    r <- RetractT ask
    lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runRetractT (f k v) r) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}

-- | Execute retractable widget with given environment.
runRetractT :: RetractT t m a -> RetractEnv t m -> m a
runRetractT (RetractT ma) e = runReaderT ma e
{-# INLINEABLE runRetractT #-}

instance (PerformEvent t m, MonadHold t m, Adjustable t m, MonadFix m, MonadIO (Performable m))
  => MonadRetract t (RetractT t m) where
  nextWidget e = do
    fire <- RetractT $ asks renvNextFire
    performEvent $ fmap (liftIO . fire) e
  {-# INLINEABLE nextWidget #-}

  retract e = do
    fire <- RetractT $ asks renvRetractFire
    performEvent $ (liftIO fire) <$ e
  {-# INLINEABLE retract #-}

  wipeRetract e = do
    fire <- RetractT $ asks renvWipeFire
    performEvent $ fmap (liftIO . fire) e
  {-# INLINEABLE wipeRetract #-}

  nextWidgetEvent = RetractT $ asks renvNextEvent
  {-# INLINABLE nextWidgetEvent #-}

  retractEvent = RetractT $ asks renvRetractEvent
  {-# INLINABLE retractEvent #-}

  wipeRetractEvent = RetractT $ asks renvWipeEvent
  {-# INLINABLE wipeRetractEvent #-}

  getRetractStack = RetractT $ asks renvStack
  {-# INLINEABLE getRetractStack #-}

  withRetractStack st (RetractT ma) = RetractT $ local (\r -> r { renvStack = st }) ma
  {-# INLINEABLE withRetractStack #-}
