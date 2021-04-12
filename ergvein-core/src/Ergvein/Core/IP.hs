module Ergvein.Core.IP(
    IP
  , SockAddr
  , parseIP
  , toSockAddr
  , makeSockAddr
  ) where

import Data.IP (IP, toSockAddr)
import Data.Text (Text, unpack)
import Network.Socket (SockAddr)
import Text.Read (readMaybe)
import Ergvein.Text

-- | Parsing IPv4 and IPv6 addresses
parseIP :: Text -> Maybe IP
parseIP = readMaybe . unpack

-- | Parsing IPv4 and IPv6 addresses and makes socket address from them
makeSockAddr :: IP -> Int -> SockAddr
makeSockAddr ip pnum = toSockAddr (ip, fromIntegral pnum)
