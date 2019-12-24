module Ergvein.Wallet.Monad.Base
  (
    MonadBaseConstr
  , MonadFrontConstr
  , MonadAlertPoster(..)
  , AlertType(..)
  , alertTypeToSeverity
  , AlertInfo(..)
  , MonadEgvLogger(..)
  , MonadClient(..)
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Text (Text)
import Ergvein.Crypto
import Data.Time(UTCTime, NominalDiffTime)
import Ergvein.Index.Client
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Native
import Language.Javascript.JSaddle
import qualified Data.Set as S
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Reflex.Localize
import Servant.Client(BaseUrl)

import Foreign.JavaScript.TH (WithJSContextSingleton)
import Reflex.Spider.Internal (SpiderHostFrame, Global)

instance MonadRandom m => MonadRandom (ReaderT e m) where
  getRandomBytes = lift . getRandomBytes
  {-# INLINE getRandomBytes #-}

-- | Type classes that we need from reflex-dom itself.
type MonadBaseConstr t m = (MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadSample t (Performable m)
  , MonadJSM (Performable m)
  , MonadIO m
  , TriggerEvent t m
  , MonadJSM m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , HasJSContext m
  , HasJSContext (Performable m)
  , HasDocument m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  , MonadRandom (Performable m)
  , PlatformNatives
  )

-- Context for unauthed widgets
-- Only to be used to request password and open the local storage
type MonadFrontConstr t m = (PlatformNatives
  , HasStoreDir m
  , HasStoreDir (Performable m)
  , MonadBaseConstr t m
  , MonadLocalized t m
  , MonadRetract t m
  , MonadAlertPoster t m
  , MonadEgvLogger t m
  , HasClientManager m
  , HasClientManager (Performable m)
  , MonadClient t m
  )

-- ===========================================================================
--           Monad EgvLogger. Implements Ervgein's logging
-- ===========================================================================

class MonadBaseConstr t m => MonadEgvLogger t m where
  -- | Internal getting of logs event and trigger
  getLogsTrigger :: m (Event t LogEntry, LogEntry -> IO ())
  -- | Get internal ref fo namespaces
  getLogsNameSpacesRef :: m (ExternalRef t [Text])

-- ===========================================================================
--           Monad Alert Poster. Implements rendering of alerts
-- ===========================================================================

-- | Different styles of alerts (including success or info messages)
data AlertType =
    AlertTypeInfo
  | AlertTypePrimary
  | AlertTypeSecondary
  | AlertTypeWarn
  | AlertTypeSuccess
  | AlertTypeFail
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Transformation from alert types to log entry types
alertTypeToSeverity :: AlertType -> LogSeverity
alertTypeToSeverity et = case et of
  AlertTypeInfo       -> LogInfo
  AlertTypePrimary    -> LogInfo
  AlertTypeSecondary  -> LogInfo
  AlertTypeWarn       -> LogWarning
  AlertTypeSuccess    -> LogInfo
  AlertTypeFail       -> LogError

-- | All info that is required to draw alert message to user
data AlertInfo = forall a . (LocalizedPrint a, Eq a) =>  AlertInfo {
  alertType       :: !AlertType -- ^ Style of message
, alertTimeout    :: !Double    -- ^ Amount of seconds the message should be shown
, alertNameSpace  :: ![Text]    -- ^ Optional name space for logs
, alertTime       :: !UTCTime   -- ^ Time of alert
, alertDoLog      :: !Bool      -- ^ Whether to log the alert or not
, alertMessage    :: !a         -- ^ Message to display
}

-- | Allows to delegate alert displaying to another widget without coupling with it
class MonadBaseConstr t m => MonadAlertPoster t m | m -> t where
  -- | Add timed alert to queue of alerts to be displayed with special widget
  postAlert :: Event t AlertInfo -> m ()
  -- | Fires when new alert arrives from 'postAlert'
  newAlertEvent :: m (Event t AlertInfo)
  -- | Get alert's event and trigger. Internal
  getAlertEventFire :: m (Event t AlertInfo, AlertInfo -> IO ())

instance MonadRandom (WithJSContextSingleton x (SpiderHostFrame Global)) where
  getRandomBytes = liftIO . getRandomBytes

-- ===========================================================================
--    Monad Client. Implements all required things for client operations
-- ===========================================================================

class (MonadBaseConstr t m, HasClientManager m, HasClientManager (Performable m)) => MonadClient t m | m -> t where
  -- | Set the number of required confirmations
  setRequiredUrlNum :: Event t (Int, Int) -> m ()
  -- | Get the number of required confirmations
  getRequiredUrlNum :: Event t () -> m (Event t (Int, Int))
  -- | Get all urls in a list
  getUrlList :: Event t () -> m (Event t [BaseUrl])
  -- | Add a number of urls to the set of valid urls
  addUrls :: Event t [BaseUrl] -> m ()
  -- | Remove a number of urls from the set of valid urls
  invalidateUrls :: Event t [BaseUrl] -> m ()
  -- | Get url reference. Internal
  getUrlsRef :: m (ExternalRef t (S.Set BaseUrl))
  -- | Get num reference. Internal
  getRequiredUrlNumRef :: m (ExternalRef t (Int, Int))
  -- | Get request timeout ref
  getRequestTimeoutRef :: m (ExternalRef t NominalDiffTime)
