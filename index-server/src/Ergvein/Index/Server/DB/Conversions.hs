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

instance Conversion DiscoveryTypes.Peer KnownPeerRecItem where
  convert Peer {..} = let
    validatedAt = pack $ show $ peerLastValidatedAt
    in KnownPeerRecItem
      { knownPeerRecAddr = convert peerAddress
      , knownPeerRecLastValidatedAt = validatedAt
      }

instance Conversion KnownPeerRecItem DiscoveryTypes.Peer where
  convert KnownPeerRecItem {..} = let
    in DiscoveryTypes.Peer
      { peerAddress = convert knownPeerRecAddr
      , peerLastValidatedAt = read $ unpack knownPeerRecLastValidatedAt
      }

instance Conversion KnownPeerRecItem Address where
  convert KnownPeerRecItem {..} = let
    in case peerAddrIP knownPeerRecAddr of
      V4 ip -> Address
        { addressType    = IPV4
        , addressPort    = peerAddrPort knownPeerRecAddr
        , addressAddress = BSL.toStrict $ toLazyByteString $ word32BE ip
        }
      V6 (a,b,c,d) -> Address
        { addressType    = IPV6
        , addressPort    = peerAddrPort knownPeerRecAddr
        , addressAddress = BSL.toStrict $ toLazyByteString $ word32BE a <> word32BE b <> word32BE c <> word32BE d
        }

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
