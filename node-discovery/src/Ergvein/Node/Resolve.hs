module Ergvein.Node.Resolve
  ( NamedSockAddr(..)
  , resolveAddr
  , resolveAddrs
  , parseHostPort
  , ipAddressParser
  , dnsLookupAddrs
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text
import Data.Function
import Data.IP
import Data.Maybe
import Data.Text (Text)
import Data.Word
import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.DNS.Types
import Data.Monoid
import Network.Socket
import Text.Read (readMaybe)
import qualified Data.List.NonEmpty as NE

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T

data NamedSockAddr = NamedSockAddr {
  namedAddrName :: Text
, namedAddrSock :: SockAddr
} deriving (Eq, Ord)

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

dnsLookup :: Resolver -> Domain -> MaybeT IO (NE.NonEmpty IP)
dnsLookup resolver domain = ((<|>) `on` MaybeT) v4 v6
  where
   v4 :: IO (Maybe (NE.NonEmpty IP))
   v4 = either (const Nothing) (NE.nonEmpty . fmap IPv4) <$> lookupA resolver domain

   v6 :: IO (Maybe (NE.NonEmpty IP))
   v6 = either (const Nothing) (NE.nonEmpty . fmap IPv6) <$> lookupAAAA resolver domain

dnsLookupAny :: Resolver -> [Domain] -> IO (Maybe (NE.NonEmpty IP))
dnsLookupAny resolver = runMaybeT . getAlt . foldMap (Alt . dnsLookup resolver)

dnsLookupAddrs :: ResolvSeed -> PortNumber -> [Domain] ->  IO (Maybe (NE.NonEmpty NamedSockAddr))
dnsLookupAddrs resolveSeed defNodePort dns = withResolver resolveSeed $ \resolver -> do
  maybeResolvedAddress <- dnsLookupAny resolver dns
  pure $ fmap (\addr -> let addrText = T.pack $ show addr in NamedSockAddr addrText $ toSockAddr $ (addr, defNodePort)) <$> maybeResolvedAddress

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
      maybeResolvedAddress <- runMaybeT $ dnsLookup resolver domain
      pure $ NamedSockAddr addressText . toSockAddr . (, port) . NE.head <$> maybeResolvedAddress
