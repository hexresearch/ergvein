module Sepulcas.Log.Monad(
    MonadNativeLogger(..)
  ) where

import Reflex
import Sepulcas.Log.Types
import Data.Text (Text)
import Reflex.ExternalRef

class (Reflex t, Monad m) => MonadNativeLogger t m | m -> t where
  -- | Internal getting of logs event and trigger
  getLogsTrigger :: m (Event t LogEntry, LogEntry -> IO ())
  -- | Get internal ref fo namespaces
  getLogsNameSpacesRef :: m (ExternalRef t [Text])
