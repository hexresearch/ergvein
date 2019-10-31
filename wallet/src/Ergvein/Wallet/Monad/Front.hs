module Ergvein.Wallet.Monad.Front(
    MonadFront(..)
  -- * Reexports
  , Text
  , MonadJSM
  , traverse_
  , module Reflex.Dom
  , module Reflex.Dom.Retractable.Class
  , module Control.Monad
  ) where

import Control.Monad
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Text (Text)
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Settings
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable.Class

class MonadFrontBase t m => MonadFront t m | m -> t where
  getSettings :: m Settings
  -- | Get loading widget trigger and fire
  getLoadingWidgetTF :: m (Event t (Text, Bool), (Text, Bool) -> IO ())
  -- | Request displaying the loading widget
  toggleLoadingWidget :: Event t (Text, Bool) -> m ()
  -- | Display loading via Dynamic
  loadingWidgetDyn :: Dynamic t (Text, Bool) -> m ()
