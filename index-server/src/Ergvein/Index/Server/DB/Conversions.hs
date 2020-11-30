module Ergvein.Index.Server.DB.Conversions where

import Conversion
import Data.ByteString.Builder
import Data.Text
import Data.Either
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.PeerDiscovery.Types as DiscoveryTypes
import Network.Socket
import Ergvein.Index.Server.DB.Schema.Indexer
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
import qualified Data.ByteString.Lazy as BSL

instance Conversion Address SockAddr where
  convert Address{..} = case addressType of
    IPV4 -> let
      port = (fromInteger $ toInteger addressPort)
      ip  =  fromRight (error "address") $ parseOnly anyWord32be addressAddress
      in SockAddrInet port ip
    IPV6 -> let
      port = (fromInteger $ toInteger addressPort)
      ip  =  fromRight (error "address") $ parseOnly ((,,,) <$> anyWord32be <*> anyWord32be <*> anyWord32be <*> anyWord32be) addressAddress
      in SockAddrInet6 port 0 ip 0
