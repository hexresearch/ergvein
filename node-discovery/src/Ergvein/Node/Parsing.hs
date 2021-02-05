module Ergvein.Node.Parsing
  ( NamedSockAddr(..)
  , parseSockAddr
  , parseSockAddrs
  ) where

import Data.Text (Text)
import Network.Socket (SockAddr)
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import Data.IP
import Data.Maybe
import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.Socket
import Text.Read (readMaybe)

import qualified Data.List.Safe as LS
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8

data NamedSockAddr = NamedSockAddr {
  namedAddrName :: Text
, namedAddrSock :: SockAddr
} deriving (Eq, Ord)

parseSockAddrs :: (MonadIO m) => ResolvSeed -> PortNumber -> [Text] -> m [NamedSockAddr]
parseSockAddrs rs defNodePort urls = liftIO $ catMaybes <$> (withResolver rs $ \r -> forM urls $ parseAddr r defNodePort)

parseSockAddr :: (MonadIO m) => ResolvSeed -> PortNumber -> Text -> m (Maybe NamedSockAddr)
parseSockAddr rs defNodePort t = liftIO $ withResolver rs $ \r -> parseAddr r defNodePort t 

parseAddr :: Resolver -> PortNumber -> Text -> IO (Maybe NamedSockAddr)
parseAddr resolver defNodePort addressText = do
  let (hostText, portText) = T.drop 1 <$> T.span (/= ':') addressText
      port = if T.null portText then defNodePort else fromMaybe defNodePort (readMaybe $ T.unpack portText)
      hostSegments = readMaybe . T.unpack <$> T.splitOn "." hostText
  case hostSegments of
    [Just a, Just b, Just c, Just d] -> pure $ Just $ NamedSockAddr addressText . SockAddrInet port $ tupleToHostAddress (a,b,c,d)
    _ -> do
      let url = B8.pack $ T.unpack hostText
      ips <- lookupA resolver url
      pure $ NamedSockAddr addressText . SockAddrInet port . toHostAddress <$> (LS.head $ fromRight mempty ips)