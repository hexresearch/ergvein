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
import Text.Read (readMaybe)
import Data.IP
import qualified Data.List.Safe as LS
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

parseSockAddrs :: (MonadIO m) => ResolvSeed -> PortNumber -> [Text] -> m [SockAddr]
parseSockAddrs rs defNodePort urls = liftIO $ catMaybes <$> (withResolver rs $ \r -> forM urls $ parseAddr r defNodePort)

parseSockAddr :: (MonadIO m) => ResolvSeed -> PortNumber -> Text -> m (Maybe SockAddr)
parseSockAddr rs defNodePort t = liftIO $ withResolver rs $ \r -> parseAddr r defNodePort t 

parseAddr :: Resolver -> PortNumber -> Text -> IO (Maybe SockAddr)
parseAddr resolver defNodePort addressText = do
  let (hostText, portText) = fmap T.tail $ T.span (/= ':') addressText
      port = if T.null portText then defNodePort else fromMaybe defNodePort (readMaybe $ T.unpack portText)
      hostSegments = readMaybe . T.unpack <$> T.splitOn "." hostText
  case hostSegments of
    [Just a, Just b, Just c, Just d] -> pure $ Just $ SockAddrInet port $ tupleToHostAddress (a,b,c,d)
    _ -> do
      let url = B8.pack $ T.unpack hostText
      ips <- lookupA resolver url
      pure $ SockAddrInet port . toHostAddress <$> (LS.head $ fromRight mempty ips)