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
  , parseSingleSockAddr
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Immortal.Worker
import Data.IP
import Data.Maybe
import Data.Time
import Network.DNS
import Network.Socket
import Reflex.ExternalRef
import Text.Read

import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings

import qualified Data.ByteString.Char8 as B8
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

-- | Parse and resolve multiple SockAddrs.
-- If the address is an IP4 tuple, it is not resolved
-- If it is not, then try to resolve it with dns lookup with the provided ResolvSeed
-- direct lookup is used instead of getAddrInfo b.c. the latter fails on Android
parseSockAddrs :: (MonadIO m, PlatformNatives) => ResolvSeed -> [Text] -> m [NamedSockAddr]
parseSockAddrs rs urls = liftIO $ do
  withResolver rs $ \resolver -> fmap catMaybes $ traverse (parseAddr resolver) urls
  where
    parseAddr :: Resolver -> Text -> IO (Maybe NamedSockAddr)
    parseAddr resolver t = do
      let (h, p) = fmap (T.drop 1) $ T.span (/= ':') t
      let port = if p == "" then defIndexerPort else fromMaybe defIndexerPort (readMaybe $ T.unpack p)
      let val = fmap (readMaybe . T.unpack) $ T.splitOn "." h
      case val of
        (Just a):(Just b):(Just c):(Just d):[] -> pure $ Just $ NamedSockAddr t $ SockAddrInet port $ tupleToHostAddress (a,b,c,d)
        _ -> do
          let url = B8.pack $ T.unpack h
          ips <- fmap (either (const []) id) $ lookupA resolver url
          case ips of
            [] -> pure Nothing
            ip:_ -> pure $ Just $ NamedSockAddr t $ SockAddrInet port (toHostAddress ip)

-- | Same as the one above, but is better for single url
-- Hides makeResolvSeed
parseSingleSockAddr :: (MonadIO m, PlatformNatives) => ResolvSeed -> Text -> m (Maybe NamedSockAddr)
parseSingleSockAddr rs t = do
  let (h, p) = fmap (T.drop 1) $ T.span (/= ':') t
  let port = if p == "" then defIndexerPort else fromMaybe defIndexerPort (readMaybe $ T.unpack p)
  let val = fmap (readMaybe . T.unpack) $ T.splitOn "." h
  case val of
    (Just a):(Just b):(Just c):(Just d):[] -> pure $ Just $ NamedSockAddr t $ SockAddrInet port $ tupleToHostAddress (a,b,c,d)
    _ -> do
      let url = B8.pack $ T.unpack h
      ips <- liftIO $ fmap (either (const []) id) $ withResolver rs (flip lookupA url)
      case ips of
        [] -> pure Nothing
        ip:_ -> pure $ Just $ NamedSockAddr t $ SockAddrInet port (toHostAddress ip)
