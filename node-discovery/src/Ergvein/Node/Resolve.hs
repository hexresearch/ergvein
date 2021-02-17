module Ergvein.Node.Resolve
  ( NamedSockAddr(..)
  , resolveAddr
  , resolveAddrs
  , parseHostPort
  , ipAddressParser
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text
import Data.Either
import Data.Function
import Data.IP
import Data.Maybe
import Data.Text (Text)
import Data.Word
import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.DNS.Types
import Network.Socket
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.Safe        as LS
import qualified Data.Text             as T

data NamedSockAddr = NamedSockAddr {
  namedAddrName :: Text
, namedAddrSock :: SockAddr
} deriving (Eq, Ord)

resolveAddrs :: (MonadIO m) => ResolvSeed -> PortNumber -> [Text] -> m [NamedSockAddr]
resolveAddrs rs defNodePort urls = liftIO $ catMaybes <$> (withResolver rs $ \r -> forM urls $ parseAddr r defNodePort)

resolveAddr :: (MonadIO m) => ResolvSeed -> PortNumber -> Text -> m (Maybe NamedSockAddr)
resolveAddr rs defNodePort t = liftIO $ withResolver rs $ \r -> parseAddr r defNodePort t 

parseAddr :: Resolver -> PortNumber -> Text -> IO (Maybe NamedSockAddr)
parseAddr resolver defNodePort addressText =
  case parseOnly (ipAddressParser defNodePort) addressText of 
    Right ip -> pure $ Just $ NamedSockAddr addressText ip
    _-> do
      let (domain, port) = parseHostPort defNodePort addressText
          v4 = LS.head . fmap IPv4 . fromRight [] <$> lookupA resolver domain
          v6 = LS.head . fmap IPv6 . fromRight [] <$> lookupAAAA resolver domain
      maybeResolvedAddress <- runMaybeT $ ((<|>) `on` MaybeT) v4 v6
      pure $ NamedSockAddr addressText . toSockAddr . (, port) <$> maybeResolvedAddress

parseHostPort :: PortNumber -> T.Text -> (Domain, PortNumber)
parseHostPort defNodePort addressText = let
  (hostString, portString) = T.drop 1 <$> T.span (/= ':') addressText
  port = fromMaybe defNodePort $ readMaybe $ T.unpack portString
  domain = B8.pack $ T.unpack hostString
  in (domain, port)

ipAddressParser :: PortNumber -> Parser SockAddr
ipAddressParser defNodePort = do
  (ip, maybePort) <- ipV4AddressParser <|> ipV6AddressParser
  endOfInput
  pure $ toSockAddr (ip, fromMaybe defNodePort maybePort)

ipV4AddressParser :: Parser (IP, Maybe PortNumber)
ipV4AddressParser = do
  let justIpV4 = (, Nothing) <$> (hostAddressParser =<< takeText)
      ipV4AndPort = (,) <$> (hostAddressParser =<< takeTill (== ':')) <*> (char ':' *> (Just <$> portParser))
  justIpV4 <|> ipV4AndPort 
  where
    hostAddressParser :: Text -> Parser IP
    hostAddressParser source = 
      case readMaybe @IPv4 . T.unpack $ source of
        Just address -> pure $ IPv4 address
        _ -> fail "not a IPv4"

ipV6AddressParser :: Parser (IP, Maybe PortNumber)
ipV6AddressParser = do
  let justIpV4 = (, Nothing) <$> (hostAddressParser =<< takeText)
      ipV4AndPort = (,) <$> (char '[' *> (hostAddressParser =<< takeTill (== ']'))) <*> (string "]:" *> (Just <$> portParser))
  justIpV4 <|> ipV4AndPort 
  where
    hostAddressParser :: Text -> Parser IP
    hostAddressParser source = 
      case readMaybe @IPv6 . T.unpack $ source of
        Just address -> pure $ IPv6 address
        _ -> fail "not a IPv6"

portParser :: Parser PortNumber
portParser = do
  port <- decimal
  if port <= toInteger (maxBound @Word16) then
      pure $ fromInteger port
    else
      fail "port value out of upper bound"