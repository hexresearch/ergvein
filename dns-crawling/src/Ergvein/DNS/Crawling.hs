module Ergvein.DNS.Crawling
  ( getDNS,
    parseSockAddr,
    parseSockAddrs
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
import Control.Monad
import Control.Monad.IO.Class
import Ergvein.DNS.Constants
import Text.Read (readMaybe)
import Data.IP
import qualified Data.List.Safe as LS
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8

class MonadIO m => DNSInfo m where
  dNodes :: m [ErgveinNodeAddr]
  dDns   :: m [HostName]
  dSeed  :: m [Domain]

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
parseSockAddrs rs urls = liftIO $ catMaybes <$> (withResolver rs $ forM urls . parseAddr) 

parseSockAddr :: (MonadIO m) => ResolvSeed -> Text -> m (Maybe SockAddr)
parseSockAddr rs t = liftIO $ withResolver rs $ flip parseAddr t

parseAddr :: Resolver -> Text -> IO (Maybe SockAddr)
parseAddr resolver addressText = do
  let (hostText, portText) = fmap T.tail $ T.span (/= ':') addressText
      port = if T.null portText then defIndexerPort else fromMaybe defIndexerPort (readMaybe $ T.unpack portText)
      hostSegments = readMaybe . T.unpack <$> T.splitOn "." hostText
  case hostSegments of
    [Just a, Just b, Just c, Just d] -> pure $ Just $ SockAddrInet port $ tupleToHostAddress (a,b,c,d)
    _ -> do
      let url = B8.pack $ T.unpack hostText
      ips <- lookupA resolver url
      pure $ SockAddrInet port . toHostAddress <$> (LS.head $ fromRight mempty ips)

initialIndexers :: DNSInfo m => m [Text]
initialIndexers = do
  nodes <- dNodes 
  dns <- dDns 
  seed <- dSeed 
  resolvInfo <- liftIO $ makeResolvSeed defaultResolvConf {
      resolvInfo = RCHostNames dns
    , resolvConcurrent = True
    }
  tryDNS <- liftIO $ getDNS resolvInfo seed
  pure $ fromMaybe nodes tryDNS