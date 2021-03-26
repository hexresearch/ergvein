-- | This module provides helper-functions to post alert messages
-- It has three groups of functions:
-- show*Msg :: (Event msg) -> m () -
-- ^ simply sends message to alert popup w/o logging the message
-- handle*Msg :: Event (Either msg val) -> m (Event val)
-- ^ sends Left occurences to alert popup and passed Right occurences through.
-- handle*Msg logs all Left occurences (!)
-- log*Msg :: Event (Either msg val) -> m (Event val)
-- ^ simply logs the Left occurences w/o sending them to alert popup. Passes Right through
-- logShow*Msg :: (Event msg) -> m ()
-- ^ both shows as an alert and logs message
-- This module also reexports common alert types and sets defaultMsgTimeout
module Ergvein.Wallet.Alert
  (
    defaultMsgTimeout
  , showDangerMsg
  , showWarnMsg
  , showSuccessMsg
  , showPrimaryMsg
  , showSecondaryMsg
  , showInfoMsg
  , showMsg
  , handleDangerMsg
  , handleWarnMsg
  , handleSuccessMsg
  , handlePrimaryMsg
  , handleSecondaryMsg
  , handleInfoMsg
  , handleAlertWith
  , logDangerMsg
  , logWarnMsg
  , logSuccessMsg
  , logPrimaryMsg
  , logSecondaryMsg
  , logInfoMsg
  , logAlertWith
  , logShowDangerMsg
  , logShowWarnMsg
  , logShowSuccessMsg
  , logShowPrimaryMsg
  , logShowSecondaryMsg
  , logShowInfoMsg
  , logShowWith
  , AlertHandler
  , module Ergvein.Wallet.Alert.Type
  ) where

import Control.Monad.IO.Class
import Data.Time (getCurrentTime)
import Ergvein.Either
import Ergvein.Wallet.Alert.Type
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Prim
import Reflex
import Reflex.ExternalRef (readExternalRef)

-- | Amount of seconds to show messages by default
defaultMsgTimeout :: Double
defaultMsgTimeout = 10

-- | Just an abbreviation
type AlertHandler t m l = (MonadBaseConstr t m, MonadLocalized t m, MonadAlertPoster t m, MonadEgvLogger t m, LocalizedPrint l, Eq l)

-- | Display localized value as error messages
showDangerMsg :: AlertHandler t m l => Event t l -> m ()
showDangerMsg = showMsg AlertTypeFail

-- | Display localized value as warning messages
showWarnMsg :: AlertHandler t m l => Event t l -> m ()
showWarnMsg = showMsg AlertTypeWarn

-- | Display localized value as success messages
showSuccessMsg :: AlertHandler t m l => Event t l -> m ()
showSuccessMsg = showMsg AlertTypeSuccess

-- | Display localized value as primary messages
showPrimaryMsg :: AlertHandler t m l => Event t l -> m ()
showPrimaryMsg = showMsg AlertTypePrimary

-- | Display localized value as secondary messages
showSecondaryMsg :: AlertHandler t m l => Event t l -> m ()
showSecondaryMsg = showMsg AlertTypeSecondary

-- | Display localized value as info messages
showInfoMsg :: AlertHandler t m l => Event t l -> m ()
showInfoMsg = showMsg AlertTypeInfo

-- | Show error message for user
showMsg :: AlertHandler t m l => AlertType -> Event t l -> m ()
showMsg et e = do
  ns <- readExternalRef =<< getLogsNameSpacesRef
  e' <- performEvent $ ffor e $ \v -> do
    t <- liftIO getCurrentTime
    pure $ AlertInfo et defaultMsgTimeout ns t False v
  postAlert e'

-- | Display 'Left' occurences as error messages
handleDangerMsg :: AlertHandler t m l => Event t (Either l a) -> m (Event t a)
handleDangerMsg = handleAlertWith AlertTypeFail

-- | Display 'Left' occurences as warning messages
handleWarnMsg :: AlertHandler t m l => Event t (Either l a) -> m (Event t a)
handleWarnMsg = handleAlertWith AlertTypeWarn

-- | Display 'Left' occurences as success messages
handleSuccessMsg :: AlertHandler t m l => Event t (Either l a) -> m (Event t a)
handleSuccessMsg = handleAlertWith AlertTypeSuccess

-- | Display 'Left' occurences as primary messages
handlePrimaryMsg :: AlertHandler t m l => Event t (Either l a) -> m (Event t a)
handlePrimaryMsg = handleAlertWith AlertTypePrimary

-- | Display 'Left' occurences as secondary messages
handleSecondaryMsg :: AlertHandler t m l => Event t (Either l a) -> m (Event t a)
handleSecondaryMsg = handleAlertWith AlertTypeSecondary

-- | Display 'Left' occurences as info messages
handleInfoMsg :: AlertHandler t m l => Event t (Either l a) -> m (Event t a)
handleInfoMsg = handleAlertWith AlertTypeInfo

-- | Display 'Left' occurences as messages
handleAlertWith :: AlertHandler t m l => AlertType -> Event t (Either l a) -> m (Event t a)
handleAlertWith et e = do
  ns <- readExternalRef =<< getLogsNameSpacesRef
  alertE <- performEvent $ fforMaybe e $ \case
    Left ge -> Just $ do
      t <- liftIO getCurrentTime
      pure $ AlertInfo et defaultMsgTimeout ns t True ge
    Right _ -> Nothing
  _ <- postAlert alertE
  pure $ fmapMaybe eitherToMaybe e

-- | Just an abbreviation
type AlertLogger t m l = (MonadBaseConstr t m, MonadLocalized t m, MonadEgvLogger t m, LocalizedPrint l, Eq l)

-- | Write to log 'Left' occurences as error messages
logDangerMsg :: AlertLogger t m l => Event t (Either l a) -> m (Event t a)
logDangerMsg = logAlertWith AlertTypeFail

-- | Write to log 'Left' occurences as warning messages
logWarnMsg :: AlertLogger t m l => Event t (Either l a) -> m (Event t a)
logWarnMsg = logAlertWith AlertTypeWarn

-- | Write to log 'Left' occurences as success messages
logSuccessMsg :: AlertLogger t m l => Event t (Either l a) -> m (Event t a)
logSuccessMsg = logAlertWith AlertTypeSuccess

-- | Write to log 'Left' occurences as primary messages
logPrimaryMsg :: AlertLogger t m l => Event t (Either l a) -> m (Event t a)
logPrimaryMsg = logAlertWith AlertTypePrimary

-- | Write to log 'Left' occurences as secondary messages
logSecondaryMsg :: AlertLogger t m l => Event t (Either l a) -> m (Event t a)
logSecondaryMsg = logAlertWith AlertTypeSecondary

-- | Write to log 'Left' occurences as info messages
logInfoMsg :: AlertLogger t m l => Event t (Either l a) -> m (Event t a)
logInfoMsg = logAlertWith AlertTypeInfo

-- | Write to log 'Left' occurences as messages
logAlertWith :: AlertLogger t m l => AlertType -> Event t (Either l a) -> m (Event t a)
logAlertWith et e = do
  ns <- readExternalRef =<< getLogsNameSpacesRef
  postLog <- fmap snd getLogsTrigger
  logE <- performEvent $ fforMaybe e $ \case
    Left ge -> Just $ do
      t <- liftIO getCurrentTime
      pure LogEntry {
          logTime = t
        , logSeverity = alertTypeToSeverity et
        , logMessage = localizedShow English ge
        , logNameSpace = ns
        }
    Right _ -> Nothing
  performEvent_ $ (liftIO . postLog) <$> logE
  pure $ fmapMaybe eitherToMaybe e

-- | Display and log localized value as error message
logShowDangerMsg :: (MonadAlertPoster t m, AlertLogger t m l) => Event t l -> m ()
logShowDangerMsg = logShowWith AlertTypeFail

-- | Display and log localized value as warning message
logShowWarnMsg :: (MonadAlertPoster t m, AlertLogger t m l) => Event t l -> m ()
logShowWarnMsg = logShowWith AlertTypeWarn

-- | Display and log localized value as success message
logShowSuccessMsg :: (MonadAlertPoster t m, AlertLogger t m l) => Event t l -> m ()
logShowSuccessMsg = logShowWith AlertTypeSuccess

-- | Display and log localized value as primary message
logShowPrimaryMsg :: (MonadAlertPoster t m, AlertLogger t m l) => Event t l -> m ()
logShowPrimaryMsg = logShowWith AlertTypePrimary

-- | Display and log localized value as secondary message
logShowSecondaryMsg :: (MonadAlertPoster t m, AlertLogger t m l) => Event t l -> m ()
logShowSecondaryMsg = logShowWith AlertTypeSecondary

-- | Display and log localized value as info message
logShowInfoMsg :: (MonadAlertPoster t m, AlertLogger t m l) => Event t l -> m ()
logShowInfoMsg = logShowWith AlertTypeInfo

-- | Display and log localized value
logShowWith :: (MonadAlertPoster t m, AlertLogger t m l) => AlertType -> Event t l -> m ()
logShowWith et e = do
    ns <- readExternalRef =<< getLogsNameSpacesRef
    postLog <- fmap snd getLogsTrigger
    e' <- performEvent $ ffor e $ \v -> do
      t <- liftIO getCurrentTime
      liftIO $ postLog $ LogEntry {
          logTime = t
        , logSeverity = alertTypeToSeverity et
        , logMessage = localizedShow English v
        , logNameSpace = ns
        }
      pure $ AlertInfo et defaultMsgTimeout ns t False v
    postAlert e'
