module Ergvein.Wallet.Monad.Base
  (
    MonadBaseConstr
  , MonadFrontBase
  , MonadBackable(..)
  , MonadErrorPoster(..)
  , ErrorType(..)
  , ErrorInfo(..)
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss)
import Reflex.Dom.Retractable
import Reflex.Localize
import Ergvein.Wallet.Native
import Ergvein.Wallet.Native

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
  )

-- | Context for unauthed widgets
-- Only to be used to request password and open the local storage
type MonadFrontBase t m = (PlatformNatives
  , HasStoreDir m
  , MonadBaseConstr t m
  , MonadLocalized t m
  , MonadRetract t m
  , MonadBackable t m
  , MonadErrorPoster t m)

-- | ===========================================================================
-- |                Monad Backable. Implements back event
-- | ===========================================================================
class MonadBaseConstr t m => MonadBackable t m | m -> t where
  -- | System back button event
  getBackEvent :: m (Event t ())

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

-- | All info that is required to draw error message to user
data ErrorInfo = forall a . (LocalizedPrint a, Eq a) =>  ErrorInfo {
  errorType    :: !ErrorType -- ^ Style of message
, errorTimeout :: !Double -- ^ Amount of seconds the message should be shown
, errorMessage :: !a -- ^ Message to display
}

-- | Allows to delegate error displaying to another widget without coupling with it
class MonadBaseConstr t m => MonadErrorPoster t m | m -> t where
  -- | Add timed error to queue of errors to be displayed with special widget
  postError :: Event t ErrorInfo -> m ()
  -- | Fires when new error arrives from 'postError'
  newErrorEvent :: m (Event t ErrorInfo)
