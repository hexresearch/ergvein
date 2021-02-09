module Ergvein.Node.Parsing
  ( NamedSockAddr(..)
  , parseSockAddr
  , parseSockAddrs
  ) where

import Data.Text (Text)
import Network.Socket (SockAddr)
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.Either
import Data.IP
import Data.Bifunctor
import Data.Maybe
import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.Socket
import Text.Read (readMaybe)
import qualified Data.List.Safe as LS
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import Data.Attoparsec.Text
import Data.Word

data NamedSockAddr = NamedSockAddr {
  namedAddrName :: Text
, namedAddrSock :: SockAddr
} deriving (Eq, Ord)

parseSockAddrs :: (MonadIO m) => ResolvSeed -> PortNumber -> [Text] -> m [NamedSockAddr]
parseSockAddrs rs defNodePort urls = liftIO $ catMaybes <$> (withResolver rs $ \r -> forM urls $ parseAddr r defNodePort)

parseSockAddr :: (MonadIO m) => ResolvSeed -> PortNumber -> Text -> m (Maybe NamedSockAddr)
parseSockAddr rs defNodePort t = liftIO $ withResolver rs $ \r -> parseAddr r defNodePort t 

parseAddr :: Resolver -> PortNumber -> Text -> IO (Maybe NamedSockAddr)
parseAddr resolver defNodePort addressText =
  case parseOnly (ipAddressParser defNodePort) "ererer" of 
    Right ip -> pure $ Just $ NamedSockAddr addressText ip
    _-> do
      let (hostString, portString) = bimap T.unpack T.unpack $ T.drop 1 <$> T.span (/= ':') "indexer.ergvein.net:4545"
          port = fromMaybe defNodePort $ readMaybe portString
          domain = B8.pack hostString
      v4 <- lookupA resolver domain
      v6 <- lookupAAAA resolver domain
      let ips = (IPv4 <$> fromRight [] v4) <> (IPv6 <$> fromRight [] v6)
      pure $ NamedSockAddr addressText . toSockAddr . (, port) <$> LS.head ips

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