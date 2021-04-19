module Ergvein.Wallet.Monad.Util
  (
    runOnMainThread
  , runOnMainThread_
  , runOnMainThreadA
  , runOnMainThreadM
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
  , mkResolvSeed
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Immortal.Worker
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.IP
import Data.Maybe
import Data.Time
import Network.DNS
import Network.Socket
import Reflex.ExternalRef
import Text.Read

import Ergvein.Node.Resolve
import Reflex.Fork
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Prim
import Sepulcas
import Sepulcas.Native
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Set as S

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

mkResolvSeed :: (MonadHasMain m, MonadHasSettings t m) => m ResolvSeed
mkResolvSeed = do
  defDns <- fmap (S.toList . settingsDns) $ readExternalRef =<< getSettingsRef
  if isAndroid
    then do
      dns <- liftIO . wait =<< runOnMainThreadA androidDetectDns
      liftIO $ makeResolvSeed defaultResolvConf {
          resolvInfo = RCHostNames $ case dns of
            [] -> defDns
            _ -> dns
        , resolvConcurrent = True
        }
  else liftIO $ do
    rs <- makeResolvSeed $ defaultResolvConf { resolvConcurrent = True}
    res :: Either SomeException () <- try $ withResolver rs $ const $ pure ()
    case res of
      Right _ -> pure rs
      Left _ ->  makeResolvSeed $ defaultResolvConf { resolvInfo = RCHostNames defDns, resolvConcurrent = True}
{-# INLINE mkResolvSeed #-}
