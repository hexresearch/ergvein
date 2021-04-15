module Sepulcas.Alert.Types where

import Reflex.Localize
import Sepulcas.Log.Types
import Data.Text (Text)
import Data.Time

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
