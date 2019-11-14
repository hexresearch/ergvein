module Ergvein.Wallet.Monad.Front(
    MonadFront(..)
  , MonadFrontBase(..)
  , AuthInfo
  , Password
  -- * Reexports
  , Text
  , MonadJSM
  , traverse_
  , module Ergvein.Wallet.Monad.Client
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
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.Data
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable.Class
import Reflex.ExternalRef

-- | Authorized context. Has access to storage and indexer's functionality
type MonadFront t m = (MonadFrontBase t m, MonadStorage t m, MonadClient t m)

type AuthInfo = ErgveinStorage
type Password = Text

class MonadFrontConstr t m => MonadFrontBase t m | m -> t where
  -- | Get current settings
  getSettings :: m Settings
  -- | Update app's settings. Sets settings to provided value and stores them
  updateSettings :: Event t Settings -> m ()
  -- | Get settings ref. Internal
  getSettingsRef :: m (ExternalRef t Settings)
  -- | Get loading widget trigger and fire. This is internal stuff
  getLoadingWidgetTF :: m (Event t (Bool, Text), (Bool, Text) -> IO ())
  -- | Request displaying the loading widget
  toggleLoadingWidget :: forall l . LocalizedPrint l => Event t (Bool, l) -> m ()
  -- | Display loading via Dynamic
  loadingWidgetDyn :: forall l . LocalizedPrint l => Dynamic t (Bool, l) -> m ()
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
  -- | Get event and trigger for pasword requesting modal. Int -- id of the request.
  getPasswordModalEF :: m (Event t Int, Int -> IO ())
  -- | Get event and trigger for the event that the password was submitted from modal. Internal
  -- Nothing value means that the modal was dismissed
  getPasswordSetEF :: m (Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
  -- | Proper requester of passwords. Use
  requestPasssword :: Event t () -> m (Event t Password)
