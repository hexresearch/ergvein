{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

-- | Widget to spam errors in popups. It is better than inplace error display as
-- it doesn't break layout of elements.
module Ergvein.Wallet.Error(
    errorHandlerWidget
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Align
import Data.Bifunctor
import Data.Coerce
import Data.Monoid
import GHC.Generics
import Reflex.Dom
import Reflex.Host.Class

import Language.Javascript.JSaddle.Types

import Data.List (find)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Reflex hiding (askEvents)

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.These as T

import Reflex.Localize
import Ergvein.Wallet.Monad.Base
import Ergvein.Text
import Ergvein.Wallet.Util
-- import Panax.Shared
-- import Panax.Frontend.Localization
-- import Panax.Frontend.Localization.Error
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Language
-- import Panax.Frontend.Monad

badge :: forall t m a . MonadFrontBase t m => Text -> m a -> m a
badge clazz = elClass "span" ("badge " <> clazz)

badgePrimary :: forall t m a . MonadFrontBase t m => m a -> m a
badgePrimary = badge "badge-primary"

-- | Helper to find element in map
findAmongMap :: (a -> Bool) -> Map k a -> Maybe (k, a)
findAmongMap f = find (f . snd) . M.toList

-- | Widget to spam errors in popups. Call it anywhere in page to start displaying
-- errors in popups.
errorHandlerWidget :: forall t m . (MonadLocalized t m, MonadErrorPoster t m, MonadFrontBase t m) => m ()
errorHandlerWidget = divClass "error-overlay" $ mdo
  langD <- getLanguage
  errE <- newErrorEvent
  logErrors errE
  let
    accumErrors :: T.These ErrorInfo [Int] -> PushM t (Map Int (Maybe (ErrorInfo, Int)))
    accumErrors v = do
      n <- sample . current $ countD
      let
        handleNewErr :: ErrorInfo -> PushM t (Map Int (Maybe (ErrorInfo, Int)))
        handleNewErr newErr@(ErrorInfo _ _ _ _ msg1) = do
          let mkNew = M.singleton n (Just (newErr, 1))
          es <- sample . current $ infosD
          l <- sample . current $ langD
          pure $ case findAmongMap (\(ErrorInfo _ _ _ _ msg2,_) -> (localizedShow l msg1 == localizedShow l msg2)) es of
            Nothing -> mkNew
            Just (i, (ei, c)) -> M.singleton i (Just (ei, c+1))
        handleDeletes :: [Int] -> PushM t (Map Int (Maybe (ErrorInfo, Int)))
        handleDeletes is = pure $ M.fromList $ (\i -> (i, Nothing)) <$> is
      case v of
        T.This newErr -> handleNewErr newErr
        T.That is -> handleDeletes is
        T.These newErr is -> do
          remm <- handleDeletes is
          addm <- handleNewErr newErr
          pure $ M.union remm addm

    accumErrorsE :: Event t (Map Int (Maybe (ErrorInfo, Int)))
    accumErrorsE = pushAlways accumErrors (align errE deleteE)

    errorWidgetD :: Int -> (ErrorInfo, Int) -> Event t (ErrorInfo, Int) -> m (Dynamic t (ErrorInfo, Int), Event t ())
    errorWidgetD _ v vE = do
      vD <- holdDyn v vE
      rD <- widgetHoldDyn $ uncurry errorWidget <$> vD
      let delE = switch . current $ rD
      pure (vD, delE)

  resD :: Dynamic t (Map Int (Dynamic t (ErrorInfo, Int), Event t ())) <- listWithKeyShallowDiff mempty accumErrorsE errorWidgetD
  let
    deletesED :: Dynamic t (Map Int (Event t ()))
    deletesED = fmap snd <$> resD

    infosD :: Dynamic t (Map Int (ErrorInfo, Int))
    infosD = joinDynThroughMap $ fmap fst <$> resD

    deleteE :: Event t [Int]
    deleteE = switch . current $ do
      es <- deletesED
      pure $ fmap M.keys . mergeMap $ es
    countD :: Dynamic t Int
    countD = length <$> deletesED
  pure ()

logErrors :: forall t m . MonadFrontBase t m => Event t ErrorInfo -> m ()
logErrors e = postLog $ ffor e $ \ErrorInfo{..} -> LogEntry {
    logTime = errorTime
  , logSeverity = errorTypeToSeverity errorType
  , logMessage = localizedShow English errorMessage
  , logNameSpace = errorNameSpace
  }
  where
        -- | Posting log message
    postLog :: MonadFrontBase t m => Event t LogEntry -> m ()
    postLog e = do
      (_, fire) <- getLogsTrigger
      performEvent_ $ ffor e $ liftIO . fire

-- | Widget that displays error to user. Fires when destruction timeout is passed.
errorWidget :: MonadFrontBase t m => ErrorInfo -> Int -> m (Event t ())
errorWidget ErrorInfo{..} n = do
  closeE <- elAttr "div" [
      ("role" , "alert")
    , ("class", "alert-popup alert alert-error-handler col-md-offset-3 col-md-6 col-lg-offset-3 col-lg-6 col-sm-12 " <> errorTypeStyle errorType)
    , ("style", "margin: 0px; border-radius: 0px; background-color: #ff931e; font-weight: normal; color: #ffffff;")
    ] $ do
      when (n > 1) $ badgePrimary $ text $ showt n
      localizedText errorMessage
      -- closeLabel <- localized ErrorClose
      -- (e,_) <- elAttr' "div" [("class", "error-close")] $ do
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
  timeoutE <- delay (realToFrac errorTimeout) =<< getPostBuild
  pure $ leftmost [timeoutE, closeE]

-- | Which style to use for specific error type
errorTypeStyle :: ErrorType -> Text
errorTypeStyle et = case et of
  ErrorTypeInfo -> "alert-info"
  ErrorTypePrimary -> "alert-primary"
  ErrorTypeSecondary -> "alert-secondary"
  ErrorTypeWarn -> "alert-warning"
  ErrorTypeSuccess -> "alert-success"
  ErrorTypeFail -> "alert-danger"

-- | Amount of seconds to show messages by default
defaultMsgTimeout :: Double
defaultMsgTimeout = 10

{-
-- | Display 'Left' occurences as error messages
handleDangerMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
handleDangerMsg = handleErrorWith' ErrorTypeFail

-- | Display 'Left' occurences as warning messages
handleWarnMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
handleWarnMsg = handleErrorWith' ErrorTypeWarn

-- | Display 'Left' occurences as success messages
handleSuccessMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
handleSuccessMsg = handleErrorWith' ErrorTypeSuccess

-- | Display 'Left' occurences as primary messages
handlePrimaryMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
handlePrimaryMsg = handleErrorWith' ErrorTypePrimary

-- | Display 'Left' occurences as secondary messages
handleSecondaryMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
handleSecondaryMsg = handleErrorWith' ErrorTypeSecondary

-- | Display 'Left' occurences as info messages
handleInfoMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
handleInfoMsg = handleErrorWith' ErrorTypeInfo

-- | Display 'Left' occurences as messages
handleErrorWith' :: (MonadErrorPoster t m, MonadFrontBase t m) => ErrorType -> Event t (Either GenericError a) -> m (Event t a)
handleErrorWith' et e = do
  ns <- getLogNameSpace
  e' <- performEvent $ ffor e $ \case
    Left ge -> do
      t <- liftIO getCurrentTime
      pure $ Left $ ErrorInfo et defaultMsgTimeout ns t ge
    Right a -> pure $ Right a
  handleErrorWith e'

-- | Write to log 'Left' occurences as error messages
logDangerMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
logDangerMsg = logErrorWith' ErrorTypeFail

-- | Write to log 'Left' occurences as warning messages
logWarnMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
logWarnMsg = logErrorWith' ErrorTypeWarn

-- | Write to log 'Left' occurences as success messages
logSuccessMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
logSuccessMsg = logErrorWith' ErrorTypeSuccess

-- | Write to log 'Left' occurences as primary messages
logPrimaryMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
logPrimaryMsg = logErrorWith' ErrorTypePrimary

-- | Write to log 'Left' occurences as secondary messages
logSecondaryMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
logSecondaryMsg = logErrorWith' ErrorTypeSecondary

-- | Write to log 'Left' occurences as info messages
logInfoMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t (Either GenericError a) -> m (Event t a)
logInfoMsg = logErrorWith' ErrorTypeInfo

-- | Write to log 'Left' occurences as messages
logErrorWith' :: (MonadErrorPoster t m, MonadFrontBase t m) => ErrorType -> Event t (Either GenericError a) -> m (Event t a)
logErrorWith' et e = do
  ns <- getLogNameSpace
  e' <- performEvent $ fforMaybe e $ \case
    Left ge -> Just $ do
      t <- liftIO getCurrentTime
      pure LogEntry {
          logTime = t
        , logSeverity = errorTypeToSeverity et
        , logMessage = localizedShow English ge
        , logNameSpace = ns
        }
    Right _ -> Nothing
  postLog e'
  pure $ fmapMaybe (either (const Nothing) Just) e

-- | Display 'Left' occurences as error messages
handleDangerMsgOwn :: (MonadErrorPoster t m, MonadLocalized t m, MonadFrontBase t m) => Event t (Either PanaxError a) -> m (Event t a)
handleDangerMsgOwn = handleErrorWithOwn ErrorTypeFail

-- | Display 'Left' occurences as warning messages
handleWarnMsgOwn :: (MonadErrorPoster t m, MonadLocalized t m, MonadFrontBase t m) => Event t (Either PanaxError a) -> m (Event t a)
handleWarnMsgOwn = handleErrorWithOwn ErrorTypeWarn

-- | Display 'Left' occurences as success messages
handleSuccessMsgOwn :: (MonadErrorPoster t m, MonadLocalized t m, MonadFrontBase t m) => Event t (Either PanaxError a) -> m (Event t a)
handleSuccessMsgOwn = handleErrorWithOwn ErrorTypeSuccess

-- | Display 'Left' occurences as primary messages
handlePrimaryMsgOwn :: (MonadErrorPoster t m, MonadLocalized t m, MonadFrontBase t m) => Event t (Either PanaxError a) -> m (Event t a)
handlePrimaryMsgOwn = handleErrorWithOwn ErrorTypePrimary

-- | Display 'Left' occurences as secondary messages
handleSecondaryMsgOwn :: (MonadErrorPoster t m, MonadLocalized t m, MonadFrontBase t m) => Event t (Either PanaxError a) -> m (Event t a)
handleSecondaryMsgOwn = handleErrorWithOwn ErrorTypeSecondary

-- | Display 'Left' occurences as info messages
handleInfoMsgOwn :: (MonadErrorPoster t m, MonadLocalized t m, MonadFrontBase t m) => Event t (Either PanaxError a) -> m (Event t a)
handleInfoMsgOwn = handleErrorWithOwn ErrorTypeInfo

-- | Display 'Left' occurences as messages
handleErrorWithOwn :: (MonadErrorPoster t m, MonadLocalized t m, MonadFrontBase t m) => ErrorType -> Event t (Either PanaxError a) -> m (Event t a)
handleErrorWithOwn et e = do
  ns <- getLogNameSpace
  let errE = fmapMaybe (either Just (const Nothing)) e
      sucE = fmapMaybe (either (const Nothing) Just) e
  errE' <- performEvent $ ffor errE $ \pe -> do
    t <- liftIO getCurrentTime
    pure $ ErrorInfo et defaultMsgTimeout ns t $ Left pe
  _ <- postError errE'
  return sucE

-- | Display 'Left' occurences as messages
handleErrorWith :: MonadErrorPoster t m => Event t (Either ErrorInfo a) -> m (Event t a)
handleErrorWith ea = do
  _ <- postError $ fmapMaybe (either Just (const Nothing)) ea
  return $ fmapMaybe (either (const Nothing) Just) ea

-- | Show error message for user
showMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => ErrorType -> Event t FrontendError -> m ()
showMsg et e = do
  ns <- getLogNameSpace
  e' <- performEvent $ ffor e $ \v -> do
    t <- liftIO getCurrentTime
    pure $ ErrorInfo et 10 ns t $ Right v
  postError e'

-- | Show error message for user
showInfoMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t FrontendError -> m ()
showInfoMsg = showMsg ErrorTypeInfo

-- | Show error message for user
showFailMsg :: (MonadErrorPoster t m, MonadFrontBase t m) => Event t FrontendError -> m ()
showFailMsg = showMsg ErrorTypeFail
-}
