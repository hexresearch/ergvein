module Ergvein.Wallet.Monad.Front(
    MonadFront(..)
  , MonadFrontBase(..)
  -- * Reexports
  , Text
  , MonadJSM
  , traverse_
  , module Reflex.Dom
  , module Reflex.Dom.Retractable.Class
  , module Control.Monad
  ) where

import Control.Concurrent.Chan
import Control.Monad
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

type MonadFront t m = (MonadFrontBase t m, MonadStorage t m)

class MonadFrontConstr t m => MonadFrontBase t m | m -> t where
  getSettings :: m Settings
  -- | Get loading widget trigger and fire
  getLoadingWidgetTF :: m (Event t (Text, Bool), (Text, Bool) -> IO ())
  -- | Request displaying the loading widget
  toggleLoadingWidget :: Event t (Text, Bool) -> m ()
  -- | Display loading via Dynamic
  loadingWidgetDyn :: Dynamic t (Text, Bool) -> m ()
  -- | System back button event
  getBackEventFire :: m (Event t (), IO ())
  -- | Internal method of getting channel where you can post actions that must be
  -- executed in main UI thread.
  getUiChan :: m (Chan (IO ()))
