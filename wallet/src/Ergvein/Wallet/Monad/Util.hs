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
  , parseSockAddrs
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Maybe
import Data.Text (pack)
import Data.Time
import Network.Socket
import Reflex.ExternalRef
import Text.Read

import Ergvein.Text
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native

import qualified Control.Exception.Safe as Ex
import qualified Control.Immortal as I
import qualified Data.Text as T

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
runOnUiThread :: MonadFrontBase t m => Event t (Performable m a) -> m (Event t a)
runOnUiThread ema = do
  ch <- getUiChan
  performEventAsync $ ffor ema $ \ma fire -> do
    unlift <- askUnliftIO
    liftIO $ writeChan ch $ fire =<< unliftIO unlift ma

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThread_ :: MonadFrontBase t m => Event t (Performable m ()) -> m ()
runOnUiThread_ ema = do
  ch <- getUiChan
  performEvent_ $ ffor ema $ \ma -> do
    unlift <- askUnliftIO
    liftIO $ writeChan ch (unliftIO unlift ma)

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

-- | Helper to parse a list of urls into a list of SockAddrs
parseSockAddrs :: MonadIO m => [Text] -> m [SockAddr]
parseSockAddrs = fmap catMaybes . traverse parseSingle
  where
    parseSingle t = do
      let (h,p) = fmap (T.drop 1) $ T.span (/= ':') t
      let p' = if p == "" then Nothing else Just $ T.unpack p
      let mport = readMaybe $ T.unpack p
      let val = fmap (readMaybe . T.unpack) $ T.splitOn "." h
      case (mport, val) of
        (Just port, (Just a):(Just b):(Just c):(Just d):[]) ->
          pure $ Just $ SockAddrInet port $ tupleToHostAddress (a,b,c,d)
        _ -> do
          let hints = defaultHints { addrFlags = [AI_ALL] , addrSocketType = Stream }
          addrs <- liftIO $ Ex.catch (
              getAddrInfo (Just hints) (Just $ T.unpack h) p'
            ) (\(_ :: Ex.SomeException) -> pure [])
          pure $ fmap addrAddress $ listToMaybe addrs
