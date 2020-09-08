module Ergvein.Index.Server.DB.Conversions where

import Conversion
import Data.ByteString.Builder
import Data.Maybe
import Data.Text
import Data.Either
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.PeerDiscovery.Types as DiscoveryTypes
import Network.Socket
import Ergvein.Index.Server.DB.Schema.Indexer
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
import qualified Data.ByteString.Lazy as BSL

instance Conversion TxInfo TxRec where
  convert txInfo = TxRec (txHash txInfo) (txHexView txInfo) (txOutputsCount txInfo)

instance Conversion DiscoveryTypes.Peer KnownPeerRecItem where
  convert Peer {..} = let
    validatedAt = pack $ show $ peerLastValidatedAt
    (port, ip) = case peerAddress of
      SockAddrInet p i -> (p, V4 i)
      SockAddrInet6 p _ i _ -> (p, V6 i)
    in KnownPeerRecItem
      { knownPeerRecAddr = DiscoveryTypes.PeerAddr ip $ fromInteger $ toInteger port
      , knownPeerRecLastValidatedAt = validatedAt
      }

instance Conversion KnownPeerRecItem DiscoveryTypes.Peer where
  convert KnownPeerRecItem {..} = let
    port = (fromInteger $ toInteger $ peerAddrPort knownPeerRecAddr)
    addr = case peerAddrIP knownPeerRecAddr of
      V4 ip -> SockAddrInet port ip
      V6 ip -> SockAddrInet6 port 0 ip 0
    in DiscoveryTypes.Peer
      { peerAddress = addr
      , peerLastValidatedAt = read $ unpack $ knownPeerRecLastValidatedAt
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