module Ergvein.Wallet.Monad(
    MonadFrontConstr
  , MonadFront(..)
  -- * Reexports
  , Text
  , void
  , traverse_
  , module Reflex.Dom
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Text (Text)
import Ergvein.Wallet.Settings
import Reflex
import Reflex.Dom

type MonadFrontConstr t m = (MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadSample t (Performable m)
  , MonadIO m
  , TriggerEvent t m
  , DomBuilderSpace m ~ GhcjsDomSpace)

class MonadFrontConstr t m => MonadFront t m | m -> t where
  getSettings :: m Settings
