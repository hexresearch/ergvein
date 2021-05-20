{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Data.Attoparsec.Text
import Data.IP
import Data.Maybe
import Data.Text
import Network.Socket
import Test.Generators
import Test.QuickCheck
import Test.QuickCheck.Instances

import Ergvein.Node.Resolve

import qualified Data.ByteString.Char8 as B8
import qualified  Data.Text as T

--------------------------------------------------------------------------
-- main
defPort :: PortNumber
defPort = 1667

testDomain :: Text
testDomain = "indexer.ergvein.com"

prop_parseHostPort :: Maybe PortNumber -> Bool
prop_parseHostPort port = parseHostPort defPort addr == (domain, fromMaybe defPort port)
  where
    addr = case port of
      Just p -> testDomain <> ":" <> (T.pack $ show p)
      _      -> testDomain
    domain = B8.pack $ T.unpack testDomain

prop_parse_ip :: IP -> Maybe PortNumber -> Bool
prop_parse_ip ip port = elem sourceAddr $ parseOnly (ipAddressParser sourcePort <* endOfInput) $ T.pack $ show sourceAddr
  where
    sourcePort = fromMaybe defPort port
    sourceAddr = toSockAddr (ip, sourcePort)

return []
main = $quickCheckAll



--------------------------------------------------------------------------
-- the end.
