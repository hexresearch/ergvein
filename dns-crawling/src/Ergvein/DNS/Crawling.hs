module Ergvein.DNS.Crawling
  ( getDNS,
    parseSockAddrs,
    parseSingleSockAddr
  )
  where

import Network.DNS.Types
import Network.DNS.Resolver
import Network.DNS.Lookup
import Data.Text (Text)
import Data.Either
import Network.Socket
import Ergvein.Text
import Data.Maybe
import Control.Monad.IO.Class
import Ergvein.DNS.Constants
import Text.Read (readMaybe)
import Data.IP
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8

getDNS :: ResolvSeed -> [Domain] -> IO (Maybe [Text])
getDNS seed domains = withResolver seed $ \resolver -> do 
  findMapMMaybe (resolve resolver) domains
  where
    resolve :: Resolver -> Domain -> IO (Maybe [Text])
    resolve resolver domain = do
        v4 <- lookupA resolver domain
        v6 <- lookupAAAA resolver domain
        let resolved = concat $ rights [(fmap showt <$> v4), (fmap showt <$> v6)]
        pure $ if length resolved < 2 then Nothing else Just resolved
    
    findMapMMaybe :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
    findMapMMaybe f (x:xs) = do
      r <- f x
      if isJust r then
        pure r
      else
        findMapMMaybe f xs
    findMapMMaybe _ [] = pure Nothing

parseSockAddrs :: (MonadIO m) => ResolvSeed -> [Text] -> m [SockAddr]
parseSockAddrs rs urls = liftIO $ do
  withResolver rs $ \resolver -> fmap catMaybes $ traverse (parseAddr resolver) urls

parseSingleSockAddr :: (MonadIO m) => ResolvSeed -> Text -> m (Maybe SockAddr)
parseSingleSockAddr rs t = do
  let (h, p) = fmap (T.drop 1) $ T.span (/= ':') t
  let port = if p == "" then defIndexerPort else fromMaybe defIndexerPort (readMaybe $ T.unpack p)
  let val = fmap (readMaybe . T.unpack) $ T.splitOn "." h
  case val of
    [Just a, Just b, Just c, Just d] -> pure $ Just $ SockAddrInet port $ tupleToHostAddress (a,b,c,d)
    _ -> do
      let url = B8.pack $ T.unpack h
      ips <- liftIO $ fmap (either (const []) id) $ withResolver rs (flip lookupA url)
      case ips of
        [] -> pure Nothing
        ip:_ -> pure $ Just $ SockAddrInet port (toHostAddress ip)

parseAddr :: Resolver -> Text -> IO (Maybe SockAddr)
parseAddr resolver t = do
  let (h, p) = fmap (T.drop 1) $ T.span (/= ':') t
  let port = if p == "" then defIndexerPort else fromMaybe defIndexerPort (readMaybe $ T.unpack p)
  let val = fmap (readMaybe . T.unpack) $ T.splitOn "." h
  case val of
    [Just a, Just b, Just c, Just d] -> pure $ Just $ SockAddrInet port $ tupleToHostAddress (a,b,c,d)
    _ -> do
      let url = B8.pack $ T.unpack h
      ips <- fmap (either (const []) id) $ lookupA resolver url
      case ips of
        [] -> pure Nothing
        ip:_ -> pure $ Just $ SockAddrInet port (toHostAddress ip)