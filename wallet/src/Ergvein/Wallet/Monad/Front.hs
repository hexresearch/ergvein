module Ergvein.Wallet.Monad.Front(
    MonadFront(..)
  , MonadFrontBase(..)
  , AuthInfo
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
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable.Class
import Reflex.ExternalRef

type MonadFront t m = (MonadFrontBase t m, MonadStorage t m)

type AuthInfo = ErgveinStorage

class MonadFrontConstr t m => MonadFrontBase t m | m -> t where
  -- | Get current settings
  getSettings :: m Settings
  -- | Update app's settings. Sets settings to provided value and stores them
  updateSettings :: Event t Settings -> m ()
  -- | Get settings ref. Internal
  getSettingsRef :: m (ExternalRef t Settings)
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
  -- | Get langRef Internal
  getLangRef :: m (ExternalRef t Language)
  -- | Return flag that comes 'True' as soon as user passes authoristion on server
  isAuthorized :: m (Dynamic t Bool)
  -- | Get authorization information that can be updated if user logs or logouts
  getAuthInfoMaybe :: m (Dynamic t (Maybe AuthInfo))
  -- | Manually set authorisation information for context. Used by widgets that
  -- implement actual login/logout. Some implementations may ingore 'Nothing'
  -- values if their semantic require persistent authorisation.
  setAuthInfo :: Event t (Maybe AuthInfo) -> m (Event t ())
