module Sepulcas.Log.Monad(
    MonadNativeLogger(..)
  , PerformLog
  , postLog
  , nameSpace
  , getLogNameSpace
  , postSeverity
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Time
import Reflex
import Reflex.ExternalRef
import Sepulcas.Log.Types

class MonadIO m => MonadNativeLogger t m | m -> t where
  -- | Internal getting of logs event and trigger
  getLogsTrigger :: m (Event t LogEntry, LogEntry -> IO ())
  -- | Get internal ref fo namespaces
  getLogsNameSpacesRef :: m (ExternalRef t [Text])

type PerformLog t m = (MonadNativeLogger t m, PerformEvent t m, MonadIO (Performable m))

-- | Posting log message
postLog :: PerformLog t m => Event t LogEntry -> m ()
postLog e = do
  (_, fire) <- getLogsTrigger
  performEvent_ $ ffor e $ liftIO . fire

-- | Wrap log name space for given widget
nameSpace :: PerformLog t m => Text -> m a -> m a
nameSpace n ma = do
  ref <- getLogsNameSpacesRef
  ns <- modifyExternalRef ref $ \ns -> (n:ns, ns)
  a <- ma
  writeExternalRef ref ns
  pure a

-- | Getting current name spaces for log
getLogNameSpace :: PerformLog t m => m [Text]
getLogNameSpace = do
  ref <- getLogsNameSpacesRef
  readExternalRef ref

-- | Helper that posts given text as log str with given severity
postSeverity :: PerformLog t m => LogSeverity -> Event t Text -> m ()
postSeverity srv e = do
  ns <- getLogNameSpace
  e' <- performEvent $ ffor e $ \msg -> do
    t <- liftIO getCurrentTime
    pure LogEntry {
        logTime = t
      , logSeverity = srv
      , logMessage = msg
      , logNameSpace = ns
      }
  postLog e'

-- | Helper that posts given text as log str with debug severity
logDebug :: PerformLog t m => Event t Text -> m ()
logDebug = postSeverity LogDebug

-- | Helper that posts given text as log str with info severity
logInfo :: PerformLog t m => Event t Text -> m ()
logInfo = postSeverity LogInfo

-- | Helper that posts given text as log str with warn severity
logWarn :: PerformLog t m => Event t Text -> m ()
logWarn = postSeverity LogWarning

-- | Helper that posts given text as log str with error severity
logError :: PerformLog t m => Event t Text -> m ()
logError = postSeverity LogError
