module Reflex.Dom.Retractable.Class(
    Retractable(..)
  , morphRetractable
  , MonadRetract(..)
  , retractStack
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Reflex
import Reflex.Network

import qualified Data.Sequence as S

-- | Information about widget that attaches information how to return to the
-- widget.
data Retractable t m = Retractable {
  -- | Which widget we are switching in
  retractableNext :: m ()
  -- | Possible return to the current widget. 'Nothing' means that the
  -- current widget is not rembered in retract stack and is forgoten.
  -- Dynamic allows to save internal state of widget on return.
, retractablePrev :: Maybe (Dynamic t (m ()))
} deriving (Generic)

-- | Helper to transform underlying monad in `Retractable`
morphRetractable :: Reflex t => (forall a . n a -> m a) -> Retractable t n -> Retractable t m
morphRetractable f (Retractable next prev) = Retractable (f next) (fmap f <$> prev)

-- | Defines context of widget that can switch control to next widget
-- and can be returned back with preserving of state.
class (MonadHold t m, MonadFix m, Reflex t, Adjustable t m) => MonadRetract t m where
  -- | Switch current widget to the next widget. Returns event that fires
  -- when the switching is about to happen.
  nextWidget :: Event t (Retractable t m) -> m (Event t ())

  -- | Switch to previous widget in stack. Returns event that fires
  -- when the switchin is about to happen.
  retract :: Event t () -> m (Event t ())

  -- | Get event that fires when any of input events in `nextWidget` is triggered.
  -- It's used for implementation of retractable stack.
  nextWidgetEvent :: m (Event t (Retractable t m))

  -- | Get event that fires when any of input events in `retract` is triggered.
  -- It's used for implementation of retractable stack.
  retractEvent :: m (Event t ())

-- | Helper ADT to merge actions with retractable stack
data StackAction t m = StackPush (Retractable t m) | StackPop

-- | All body of the widget will be rerendered when some subcomputation emits switching event.
retractStack :: forall t m . MonadRetract t m => m () -> m (Event t ())
retractStack ma = do
  nextE <- fmap StackPush <$> nextWidgetEvent
  backE <- fmap (const StackPop) <$> retractEvent
  let actionE = leftmost [nextE, backE]
  let go :: StackAction t m -> [Retractable t m] -> [Retractable t m]
      go a rs = case a of
        StackPush r -> maybe rs (const $ r : rs) $ retractablePrev r
        StackPop -> drop 1 rs
  stackD :: Dynamic t [Retractable t m] <- foldDyn go [] actionE
  resD <- networkHold ma $ flip pushAlways actionE $ \case
    StackPush r -> pure $ retractableNext r
    StackPop -> do
      rs <- sample . current $ stackD
      case rs of
        (r : _) | Just maD <- retractablePrev r -> sample . current $ maD
        _ -> pure $ ma
  pure $ updated resD

instance MonadRetract t m => MonadRetract t (ReaderT r m) where
  nextWidget e = do
    r <- ask
    lift $ nextWidget (morphRetractable (flip runReaderT r) <$> e)
  retract = lift . retract
  nextWidgetEvent = do
    e <- lift nextWidgetEvent
    pure $ morphRetractable lift <$> e
  retractEvent = lift retractEvent
