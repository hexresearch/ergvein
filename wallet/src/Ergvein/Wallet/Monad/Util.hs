module Ergvein.Wallet.Monad.Util
  (
    runOnUiThread
  , runOnUiThread_
  , runOnUiThreadA
  , runOnUiThreadM
  , nameSpace
  , getLogNameSpace
  , postSeverity
  , logDebug
  , logInfo
  , logWarn
  , logError
  , postLog
  , performFork
  , performFork_
  , worker
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Text (pack)
import Data.Time
import Ergvein.Text
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Foreign.JavaScript.TH (WithJSContextSingleton(..))
import Reflex.ExternalRef
import Reflex.Spider.Internal (SpiderHostFrame(..), EventM(..))

import qualified Control.Immortal as I

-- | Posting log message
postLog :: MonadEgvLogger t m => Event t LogEntry -> m ()
postLog e = do
  (_, fire) <- getLogsTrigger
  performEvent_ $ ffor e $ liftIO . fire

-- | Wrap log name space for given widget
nameSpace :: MonadEgvLogger t m => Text -> m a -> m a
nameSpace n ma = do
  ref <- getLogsNameSpacesRef
  ns <- modifyExternalRef ref $ \ns -> (n:ns, ns)
  a <- ma
  writeExternalRef ref ns
  pure a

-- | Getting current name spaces for log
getLogNameSpace :: MonadEgvLogger t m => m [Text]
getLogNameSpace = do
  ref <- getLogsNameSpacesRef
  readExternalRef ref

-- | Helper that posts given text as log str with given severity
postSeverity :: MonadEgvLogger t m => LogSeverity -> Event t Text -> m ()
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
logDebug :: MonadEgvLogger t m => Event t Text -> m ()
logDebug = postSeverity LogDebug

-- | Helper that posts given text as log str with info severity
logInfo :: MonadEgvLogger t m => Event t Text -> m ()
logInfo = postSeverity LogInfo

-- | Helper that posts given text as log str with warn severity
logWarn :: MonadEgvLogger t m => Event t Text -> m ()
logWarn = postSeverity LogWarning

-- | Helper that posts given text as log str with error severity
logError :: MonadEgvLogger t m => Event t Text -> m ()
logError = postSeverity LogError

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThread :: MonadFrontBase t m => Event t (IO a) -> m (Event t a)
runOnUiThread ema = do
  ch <- getUiChan
  performEventAsync $ ffor ema $ \ma fire -> liftIO $ writeChan ch $ fire =<< ma

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThread_ :: MonadFrontBase t m => Event t (IO ()) -> m ()
runOnUiThread_ ema = do
  ch <- getUiChan
  performEvent_ $ ffor ema $ \ma -> liftIO $ writeChan ch ma

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThreadA :: MonadFrontBase t m => IO a -> m (Async a)
runOnUiThreadA ma = do
  ch <- getUiChan
  liftIO $ do
    resVar <- newEmptyMVar
    writeChan ch $ putMVar resVar =<< ma
    async $ takeMVar resVar

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThreadM :: MonadFrontBase t m => IO () -> m ()
runOnUiThreadM ma = do
  ch <- getUiChan
  liftIO $ writeChan ch ma

-- | Helper that starts new immortal thread with logging of errors
worker :: (MonadUnliftIO m, PlatformNatives) => String -> (I.Thread -> m ()) -> m I.Thread
worker lbl f = I.createWithLabel lbl $ \thread -> I.onUnexpectedFinish thread logthem (f thread)
  where
    logthem me = case me of
      Left e -> do
        logWrite $ "Worker " <> pack lbl <> " exit with: " <> showt e
        liftIO $ threadDelay 1000000
      _ -> pure ()
