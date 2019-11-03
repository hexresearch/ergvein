{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

-- | Widget to spam alerts in popups. It is better than inplace alert display as
-- it doesn't break layout of elements.
module Ergvein.Wallet.Alert(
    alertHandlerWidget
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
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Align
import Data.Bifunctor
import Data.Coerce
import Data.Monoid
import Data.Time
import GHC.Generics
import Reflex.Dom
import Reflex.Host.Class

import Language.Javascript.JSaddle.Types

import Data.List (find)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Reflex hiding (askEvents)
import Reflex.ExternalRef(readExternalRef)

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.These as T

import Ergvein.Text
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Util

badge :: forall t m a . MonadFrontBase t m => Text -> m a -> m a
badge clazz = elClass "span" ("badge " <> clazz)

badgePrimary :: forall t m a . MonadFrontBase t m => m a -> m a
badgePrimary = badge "badge-primary"

-- | Helper to find element in map
findAmongMap :: (a -> Bool) -> Map k a -> Maybe (k, a)
findAmongMap f = find (f . snd) . M.toList

-- | Widget to spam alerts in popups. Call it anywhere in page to start displaying
-- alerts in popups.
alertHandlerWidget :: forall t m . (MonadLocalized t m, MonadAlertPoster t m, MonadFrontBase t m) => m ()
alertHandlerWidget = divClass "alert-overlay" $ mdo
  langD <- getLanguage
  errE <- newAlertEvent
  logAlerts errE
  let
    accumAlerts :: T.These AlertInfo [Int] -> PushM t (Map Int (Maybe (AlertInfo, Int)))
    accumAlerts v = do
      n <- sample . current $ countD
      let
        handleNewErr :: AlertInfo -> PushM t (Map Int (Maybe (AlertInfo, Int)))
        handleNewErr newErr@(AlertInfo _ _ _ _ msg1) = do
          let mkNew = M.singleton n (Just (newErr, 1))
          es <- sample . current $ infosD
          l <- sample . current $ langD
          pure $ case findAmongMap (\(AlertInfo _ _ _ _ msg2,_) -> (localizedShow l msg1 == localizedShow l msg2)) es of
            Nothing -> mkNew
            Just (i, (ei, c)) -> M.singleton i (Just (ei, c+1))
        handleDeletes :: [Int] -> PushM t (Map Int (Maybe (AlertInfo, Int)))
        handleDeletes is = pure $ M.fromList $ (\i -> (i, Nothing)) <$> is
      case v of
        T.This newErr -> handleNewErr newErr
        T.That is -> handleDeletes is
        T.These newErr is -> do
          remm <- handleDeletes is
          addm <- handleNewErr newErr
          pure $ M.union remm addm

    accumAlertsE :: Event t (Map Int (Maybe (AlertInfo, Int)))
    accumAlertsE = pushAlways accumAlerts (align errE deleteE)

    alertWidgetD :: Int -> (AlertInfo, Int) -> Event t (AlertInfo, Int) -> m (Dynamic t (AlertInfo, Int), Event t ())
    alertWidgetD _ v vE = do
      vD <- holdDyn v vE
      rD <- widgetHoldDyn $ uncurry alertWidget <$> vD
      let delE = switch . current $ rD
      pure (vD, delE)

  resD :: Dynamic t (Map Int (Dynamic t (AlertInfo, Int), Event t ())) <- listWithKeyShallowDiff mempty accumAlertsE alertWidgetD
  let
    deletesED :: Dynamic t (Map Int (Event t ()))
    deletesED = fmap snd <$> resD

    infosD :: Dynamic t (Map Int (AlertInfo, Int))
    infosD = joinDynThroughMap $ fmap fst <$> resD

    deleteE :: Event t [Int]
    deleteE = switch . current $ do
      es <- deletesED
      pure $ fmap M.keys . mergeMap $ es
    countD :: Dynamic t Int
    countD = length <$> deletesED
  pure ()

logAlerts :: forall t m . MonadFrontBase t m => Event t AlertInfo -> m ()
logAlerts e = postLog $ ffor e $ \AlertInfo{..} -> LogEntry {
    logTime = alertTime
  , logSeverity = alertTypeToSeverity alertType
  , logMessage = localizedShow English alertMessage
  , logNameSpace = alertNameSpace
  }
  where
        -- | Posting log message
    postLog :: MonadFrontBase t m => Event t LogEntry -> m ()
    postLog e = do
      (_, fire) <- getLogsTrigger
      performEvent_ $ ffor e $ liftIO . fire

-- | Widget that displays alert to user. Fires when destruction timeout is passed.
alertWidget :: MonadFrontBase t m => AlertInfo -> Int -> m (Event t ())
alertWidget AlertInfo{..} n = do
  closeE <- elAttr "div" [
      ("role" , "alert")
    , ("class", "alert-popup alert alert-handler col-md-offset-3 col-md-6 col-lg-offset-3 col-lg-6 col-sm-12 " <> alertTypeStyle alertType)
    , ("style", "margin: 0px; border-radius: 0px; background-color: #ff931e; font-weight: normal; color: #ffffff;")
    ] $ do
      when (n > 1) $ badgePrimary $ text $ showt n
      localizedText alertMessage
      -- closeLabel <- localized AlertClose
      -- (e,_) <- elAttr' "div" [("class", "alert-close")] $ do
      --   let attrs = do
      --         label <- closeLabel
      --         pupx -> %, lots of changes in positions and width, added steaky header,…re [
      --             ("href", "#")
      --           , ("onclick", "return false;")
      --           , ("data-tippy-placement", "top")
      --           , ("title", label)
      --           , ("class","fas fa-times")
      --           ]
      --   elDynAttr "i" attrs $ pure ()
      -- pure $ domEvent Click e
      pure never
  timeoutE <- delay (realToFrac alertTimeout) =<< getPostBuild
  pure $ leftmost [timeoutE, closeE]

-- | Which style to use for specific alert type
alertTypeStyle :: AlertType -> Text
alertTypeStyle et = case et of
  AlertTypeInfo -> "alert-info"
  AlertTypePrimary -> "alert-primary"
  AlertTypeSecondary -> "alert-secondary"
  AlertTypeWarn -> "alert-warning"
  AlertTypeSuccess -> "alert-success"
  AlertTypeFail -> "alert-danger"

-- | Amount of seconds to show messages by default
defaultMsgTimeout :: Double
defaultMsgTimeout = 10

-- | Just an abbreviation
type AlertHandler t m l = (MonadBaseConstr t m, MonadLocalized t m, MonadAlertPoster t m, MonadEgvLogger t m, LocalizedPrint l, Eq l)

-- | Display 'Left' occurences as error messages
showDangerMsg :: AlertHandler t m l => Event t l -> m ()
showDangerMsg = showMsg AlertTypeFail

-- | Display 'Left' occurences as warning messages
showWarnMsg :: AlertHandler t m l => Event t l -> m ()
showWarnMsg = showMsg AlertTypeWarn

-- | Display 'Left' occurences as success messages
showSuccessMsg :: AlertHandler t m l => Event t l -> m ()
showSuccessMsg = showMsg AlertTypeSuccess

-- | Display 'Left' occurences as primary messages
showPrimaryMsg :: AlertHandler t m l => Event t l -> m ()
showPrimaryMsg = showMsg AlertTypePrimary

-- | Display 'Left' occurences as secondary messages
showSecondaryMsg :: AlertHandler t m l => Event t l -> m ()
showSecondaryMsg = showMsg AlertTypeSecondary

-- | Display 'Left' occurences as info messages
showInfoMsg :: AlertHandler t m l => Event t l -> m ()
showInfoMsg = showMsg AlertTypeInfo

-- | Show error message for user
showMsg :: AlertHandler t m l => AlertType -> Event t l -> m ()
showMsg et e = do
  ns <- readExternalRef =<< getLogsNameSpacesRef
  e' <- performEvent $ ffor e $ \v -> do
    t <- liftIO getCurrentTime
    pure $ AlertInfo et defaultMsgTimeout ns t v
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
      pure $ AlertInfo et defaultMsgTimeout ns t ge
    Right _ -> Nothing
  _ <- postAlert alertE
  pure $ fmapMaybe (either (const Nothing) Just) e

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
  pure $ fmapMaybe (either (const Nothing) Just) e
