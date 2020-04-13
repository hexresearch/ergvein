module Ergvein.Wallet.Monad.Front(
    MonadFront(..)
  , MonadFrontBase(..)
  , MonadFrontAuth(..)
  , AuthInfo(..)
  , Password
  , getCurrentHeight
  , setCurrentHeight
  , getFiltersSync
  -- * Reexports
  , Text
  , MonadJSM
  , traverse_
  , module Ergvein.Wallet.Monad.Base
  , module Reflex.Dom
  , module Reflex.Dom.Retractable.Class
  , module Control.Monad
  ) where

import Control.Concurrent.Chan
import Control.Monad
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Servant.Client(BaseUrl)
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable.Class
import Reflex.ExternalRef

import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Headers.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Status

import qualified Data.Map.Strict as M

-- | Authorized context. Has access to storage and indexer's functionality
type MonadFront t m = (
    MonadFrontAuth t m
  , MonadStorage t m
  , MonadClient t m
  , HasHeadersStorage m
  , HasFiltersStorage m
  , HasFiltersStorage (Performable m)
  )

class MonadFrontBase t m => MonadFrontAuth t m | m -> t where
  -- | Set global sync process value each time the event is fired
  setSyncProgress :: Event t SyncProgress -> m ()
  -- | Get global sync process value
  getSyncProgress :: m (Dynamic t SyncProgress)
  -- | Internal method.
  getSyncProgressRef :: m (ExternalRef t SyncProgress)
  -- | Internal method to get reference with known heights per currency.
  getHeightRef :: m (ExternalRef t (Map Currency Integer))
  -- | Internal method to get flag if we has fully synced filters at the moment.
  getFiltersSyncRef :: m (ExternalRef t (Map Currency Bool))
  -- | Get activeCursRef Internal
  getActiveCursRef :: m (ExternalRef t ActiveCurrencies)
  -- | Get auth info. Not a Maybe since this is authorized context
  getAuthInfo :: m (Dynamic t AuthInfo)

class MonadFrontConstr t m => MonadFrontBase t m | m -> t where
  -- | Get current settings
  getSettings :: m Settings
  -- | Get current settings dynamic
  getSettingsD :: m (Dynamic t Settings)
  -- | Update app's settings. Sets settings to provided value and stores them
  updateSettings :: Event t Settings -> m (Event t ())
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
  -- | Internal method to get storage of auth info
  getAuthInfoRef :: m (ExternalRef t (Maybe AuthInfo))

-- | Get current value of longest chain height for given currency.
getCurrentHeight :: MonadFrontAuth t m => Currency -> m Integer
getCurrentHeight c = do
  r <- getHeightRef
  m <- readExternalRef r
  pure $ fromMaybe 0 $ M.lookup c m

-- | Update current height of longest chain for given currency. TODO: migrate this to direct asking BTC nodes, not indexer.
setCurrentHeight :: MonadFrontAuth t m => Currency -> Event t Integer -> m ()
setCurrentHeight c e = do
  r <- getHeightRef
  performEvent_ $ ffor e $ \h -> modifyExternalRef r ((, ()) . M.insert c h)

-- | Get current value that tells you whether filters are fully in sync now or not
getFiltersSync :: MonadFrontAuth t m => Currency -> m (Dynamic t Bool)
getFiltersSync c = do
  d <- externalRefDynamic =<< getFiltersSyncRef
  pure $ fromMaybe False . M.lookup c <$> d
