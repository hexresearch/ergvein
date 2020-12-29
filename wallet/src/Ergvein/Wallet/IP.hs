{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.IP(
    IP
  , SockAddr
  , parseIP
  , toSockAddr
  , makeSockAddr
  ) where

import Data.IP (IP, toSockAddr)
import Data.Text (Text, unpack)
import Ergvein.Wallet.Elements.Input.Class
import Network.Socket (SockAddr)
import Text.Read (readMaybe)
import Ergvein.Text

import Ergvein.Wallet.Localization.IP

-- | Parsing IPv4 and IPv6 addresses
parseIP :: Text -> Maybe IP
parseIP = readMaybe . unpack

-- | Parsing IPv4 and IPv6 addresses and makes socket address from them
makeSockAddr :: IP -> Int -> SockAddr
makeSockAddr ip pnum = toSockAddr (ip, fromIntegral pnum)

instance (LocalizedPrint l, Wrappable IPStrings l) => Inputable l IP where
  displayInput _ = showt
  parseInput = maybe (Left $ wrap IPParseFailed) Right . parseIP
