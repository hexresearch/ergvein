module Ergvein.Index.Server.DB.Conversions where

import Conversion
import Data.ByteString.Builder
import Data.Maybe
import Data.Text
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.PeerDiscovery.Types as DiscoveryTypes
import Network.Socket
import Servant.Client.Core
import Ergvein.Index.Server.DB.Schema.Indexer
import qualified Data.ByteString.Lazy as BSL

instance Conversion TxInfo TxRec where
  convert txInfo = TxRec (txHash txInfo) (txHexView txInfo) (txOutputsCount txInfo)

instance Conversion DiscoveryTypes.Peer1 KnownPeerRecItem where
  convert Peer1 {..} = let
    validatedAt = pack $ show $ peerLastValidatedAt1
    (port, ip) = case peerAddress of
      SockAddrInet p i -> (p, V4 i)
      SockAddrInet6 p _ i _ -> (p, V6 i)
    in KnownPeerRecItem
      { knownPeerRecIP = ip
      , knownPeerRecPort = fromInteger $ toInteger port
      , knownPeerRecLastValidatedAt = validatedAt
      }

instance Conversion KnownPeerRecItem DiscoveryTypes.Peer1 where
  convert KnownPeerRecItem {..} = let
    port = (fromInteger $ toInteger knownPeerRecPort)
    addr = case knownPeerRecIP of
      V4 ip -> SockAddrInet port ip
      V6 ip -> SockAddrInet6 port 0 ip 0
    in DiscoveryTypes.Peer1
      { peerAddress = addr
      , peerLastValidatedAt1 = read $ unpack $ knownPeerRecLastValidatedAt
      }

instance Conversion KnownPeerRecItem Address where
  convert KnownPeerRecItem {..} = let
    in case knownPeerRecIP of
      V4 ip -> Address
        { addressType    = IPV4
        , addressPort    = knownPeerRecPort
        , addressAddress = BSL.toStrict $ toLazyByteString $ word32BE ip
        }
      V6 (a,b,c,d) -> Address
        { addressType    = IPV6
        , addressPort    = knownPeerRecPort
        , addressAddress = BSL.toStrict $ toLazyByteString $ word32BE a <> word32BE b <> word32BE c <> word32BE d
        } 
