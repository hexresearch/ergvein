module Ergvein.Wallet.Monad(
    MonadFrontConstr
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

type MonadFrontConstr t m = (MonadHold t m
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
  , MonadRetract t m)

class MonadFrontConstr t m => MonadFront t m | m -> t where
  getSettings :: m Settings
  -- | System back button event
  getBackEvent :: m (Event t ())
