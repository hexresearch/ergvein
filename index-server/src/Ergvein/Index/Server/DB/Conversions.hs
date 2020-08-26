module Ergvein.Index.Server.DB.Conversions where

import Conversion
import Data.Text
import Data.Maybe
import Servant.Client.Core
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Schema
import Network.Socket
import Ergvein.Index.Server.PeerDiscovery.Types as DiscoveryTypes

instance Conversion TxInfo TxRec where
  convert txInfo = TxRec (txHash txInfo) (txHexView txInfo) (txOutputsCount txInfo)

instance Conversion DiscoveryTypes.Peer KnownPeerRecItem where
  convert peer = KnownPeerRecItem
    { knownPeerRecUrl = pack $ showBaseUrl $ peerUrl peer
    , knownPeerRecLastValidatedAt = pack $ show $ peerLastValidatedAt peer 
    , knownPeerRecIsSecureConn =  
        case peerConnScheme peer of
              Https -> True
              Http -> False
    }

instance Conversion DiscoveryTypes.Peer1 KnownPeerRecItem1 where
  convert Peer1 {..} = let
    validatedAt = pack $ show $ peerLastValidatedAt1
    (port, ip) = case peerAddress of
      SockAddrInet p i -> (p, V4 i)
      SockAddrInet6 p _ i _ -> (p, V6 i)
    in KnownPeerRecItem1
      { knownPeerRecIP = ip
      , knownPeerRecPort = fromInteger $ toInteger port
      , knownPeerRecLastValidatedAt1 = validatedAt
      }

instance Conversion KnownPeerRecItem DiscoveryTypes.Peer where
  convert peer = DiscoveryTypes.Peer
    { peerUrl = fromJust $ parseBaseUrl $ unpack $ knownPeerRecUrl peer
    , peerLastValidatedAt = read $ unpack $ knownPeerRecLastValidatedAt peer
    , peerConnScheme = 
        case knownPeerRecIsSecureConn peer of
                True  -> Https
                False -> Http
    }

instance Conversion KnownPeerRecItem1 DiscoveryTypes.Peer1 where
  convert KnownPeerRecItem1 {..} = let
    port = (fromInteger $ toInteger knownPeerRecPort)
    addr = case knownPeerRecIP of
      V4 ip -> SockAddrInet port ip
      V6 ip -> SockAddrInet6 port 0 ip 0
    in DiscoveryTypes.Peer1
      { peerAddress = addr
      , peerLastValidatedAt1 = read $ unpack $ knownPeerRecLastValidatedAt1
      }