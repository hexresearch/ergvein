module Ergvein.Wallet.Monad(
    MonadBaseConstr
  , MonadFrontConstr
  , MonadFront(..)
  -- * Reexports
  , Text
  , MonadJSM
  , traverse_
  , module Reflex.Dom
  , module Reflex.Dom.Retractable.Class
  , module Control.Monad
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Text (Text)
import Ergvein.Wallet.Settings
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable.Class

-- | Type classes that we need from reflex-dom itself.
type MonadBaseConstr t m = (MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadSample t (Performable m)
  , MonadIO m
  , TriggerEvent t m
  , MonadJSM m
  , DomBuilderSpace m ~ GhcjsDomSpace
  )

-- | Additional type classes for widgets API. There are contexts that
-- cannot be derived from raw reflex-dom context.
type MonadFrontConstr t m = (MonadBaseConstr t m
  , MonadRetract t m
  )

class MonadFrontConstr t m => MonadFront t m | m -> t where
  getSettings :: m Settings
  -- | System back button event
  getBackEvent :: m (Event t ())
