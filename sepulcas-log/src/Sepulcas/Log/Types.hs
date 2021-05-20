module Sepulcas.Log.Types(
    LogSeverity(..)
  , LogEntry(..)
  , logStorageKey
  , logStorageKeyOld
  , logStorageMaxSize
  ) where

import Data.Aeson
import Data.Text (Text, unpack)
import Data.Time

-- | Severity of log message to filter them
data LogSeverity = LogDebug | LogError | LogWarning | LogInfo
  deriving (Eq, Show, Read)

instance ToJSON LogSeverity where
  toJSON v = String $ case v of
    LogDebug -> "debug"
    LogError -> "error"
    LogWarning -> "warn"
    LogInfo -> "info"
  {-# INLINE toJSON #-}

instance FromJSON LogSeverity where
  parseJSON = withText "LogSeverity" $ \v -> maybe (fail $ "Unknown LogSeverity" <> unpack v) pure $ case v of
    "debug" -> Just LogDebug
    "error" -> Just LogError
    "warn" -> Just LogWarning
    "info" -> Just LogInfo
    _ -> Nothing
  {-# INLINE parseJSON #-}

-- | Structured log entry
data LogEntry = LogEntry {
  logTime      :: !UTCTime      -- ^ Time of message
, logSeverity  :: !LogSeverity  -- ^ Severity of message
, logMessage   :: !Text         -- ^ Message payload
, logNameSpace :: ![Text]       -- ^ Namespace stack of message (widget)
} deriving (Show)

instance ToJSON LogEntry where
  toJSON LogEntry{..} = object [
      "t"   .= logTime
    , "sev" .= logSeverity
    , "msg" .= logMessage
    , "ns"  .= logNameSpace
    ]
  {-# INLINE toJSON #-}

instance FromJSON LogEntry where
  parseJSON = withObject "LogEntry" $ \o -> LogEntry
    <$> o .: "t"
    <*> o .: "sev"
    <*> o .: "msg"
    <*> o .: "ns"
  {-# INLINE parseJSON #-}

-- | Internal filename for logs
logStorageKey :: Text
logStorageKey = "ergvein_logs"

-- | Internal filename for archived logs
logStorageKeyOld :: Text
logStorageKeyOld = "ergvein_logs_old"

-- | Amount of bytes in store allowed for logs
logStorageMaxSize :: Int
logStorageMaxSize = 10 * 1024 * 1024 -- 10 kbyte
