module Ergvein.Wallet.Monad(
    MonadBaseConstr
  , MonadFrontConstr
  , MonadStorage(..)
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
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Settings
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable.Class
import Reflex.Localize

-- | Additional type classes for widgets API. There are contexts that
-- cannot be derived from raw reflex-dom context.
type MonadFrontConstr t m = (MonadBaseConstr t m
  , MonadRetract t m
  , MonadLocalized t m
  , MonadStorage t m
  )

class MonadFrontConstr t m => MonadFront t m | m -> t where
  getSettings :: m Settings
  -- | System back button event
  getBackEvent :: m (Event t ())
  -- | Get loading widget trigger and fire
  getLoadingWidgetTF :: m (Event t (Text, Bool), (Text, Bool) -> IO ())
  -- | Request displaying the loading widget
  toggleLoadingWidget :: Event t (Text, Bool) -> m ()
  -- | Display loading via Dynamic
  loadingWidgetDyn :: Dynamic t (Text, Bool) -> m ()
