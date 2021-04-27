module Ergvein.Core.IP(
    IP
  , SockAddr
  , parseIP
  , toSockAddr
  , makeSockAddr
  , parseSockAddrs
  , parseSingleSockAddr
  ) where

import Control.Monad.IO.Class
import Data.IP (IP, toSockAddr, toHostAddress)
import Data.Maybe
import Data.Text (Text, unpack)
import Ergvein.Core.Settings.Constants
import Ergvein.Node.Resolve
import Network.DNS
import Network.Socket
import Sepulcas.Native
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

-- | Parsing IPv4 and IPv6 addresses
parseIP :: Text -> Maybe IP
parseIP = readMaybe . unpack

-- | Parsing IPv4 and IPv6 addresses and makes socket address from them
makeSockAddr :: IP -> Int -> SockAddr
makeSockAddr ip pnum = toSockAddr (ip, fromIntegral pnum)

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
