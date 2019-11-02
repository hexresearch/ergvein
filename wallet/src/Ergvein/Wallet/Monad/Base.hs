module Ergvein.Wallet.Monad.Base
  (
    MonadBaseConstr
  , MonadFrontConstr
  , MonadErrorPoster(..)
  , ErrorType(..)
  , errorTypeToSeverity
  , ErrorInfo(..)
  , MonadEgvLogger(..)
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Text (Text)
import Data.Time(UTCTime)
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Native
import Ergvein.Wallet.Native
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Reflex.Localize

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
  , HasJSContext m
  , HasJSContext (Performable m)
  , HasDocument m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  )

-- | Context for unauthed widgets
-- Only to be used to request password and open the local storage
type MonadFrontConstr t m = (PlatformNatives
  , HasStoreDir m
  , MonadBaseConstr t m
  , MonadLocalized t m
  , MonadRetract t m
  , MonadErrorPoster t m
  , MonadEgvLogger t m)

-- | ===========================================================================
-- |           Monad EgvLogger. Implements Ervgein's logging
-- | ===========================================================================

class MonadBaseConstr t m => MonadEgvLogger t m where
    -- | Internal getting of logs event and trigger
  getLogsTrigger :: m (Event t LogEntry, LogEntry -> IO ())
  -- | Get internal ref fo namespaces
  getLogsNameSpacesRef :: m (ExternalRef t [Text])

-- | ===========================================================================
-- |           Monad Error Poster. Implements rendering of errors
-- | ===========================================================================

-- | Different styles of errors (including success or info messages)
data ErrorType =
    ErrorTypeInfo
  | ErrorTypePrimary
  | ErrorTypeSecondary
  | ErrorTypeWarn
  | ErrorTypeSuccess
  | ErrorTypeFail
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Transformation from error types to log entry types
errorTypeToSeverity :: ErrorType -> LogSeverity
errorTypeToSeverity et = case et of
  ErrorTypeInfo -> LogInfo
  ErrorTypePrimary -> LogInfo
  ErrorTypeSecondary -> LogInfo
  ErrorTypeWarn -> LogWarning
  ErrorTypeSuccess -> LogInfo
  ErrorTypeFail -> LogError

-- | All info that is required to draw error message to user
data ErrorInfo = forall a . (LocalizedPrint a, Eq a) =>  ErrorInfo {
  errorType       :: !ErrorType -- ^ Style of message
, errorTimeout    :: !Double -- ^ Amount of seconds the message should be shown
, errorNameSpace  :: ![Text] -- ^ Optional name space for logs
, errorTime       :: !UTCTime -- ^ Time of error
, errorMessage    :: !a -- ^ Message to display
}

-- | Allows to delegate error displaying to another widget without coupling with it
class MonadBaseConstr t m => MonadErrorPoster t m | m -> t where
  -- | Add timed error to queue of errors to be displayed with special widget
  postError :: Event t ErrorInfo -> m ()
  -- | Fires when new error arrives from 'postError'
  newErrorEvent :: m (Event t ErrorInfo)
  -- | Get error's event and trigger. Internal
  getErrorEventFire :: m (Event t ErrorInfo, ErrorInfo -> IO ())
