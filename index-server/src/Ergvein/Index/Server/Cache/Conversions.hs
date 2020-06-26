module Ergvein.Index.Server.Cache.Conversions where

import Conversion
import Data.Text
import Data.Maybe
import Servant.Client.Core
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Index.Server.PeerDiscovery.Types as DiscoveryTypes

instance Conversion TxInfo TxCacheRec where
  convert txInfo = TxCacheRec (txHash txInfo) (txHexView txInfo) (txOutputsCount txInfo)

instance Conversion DiscoveryTypes.Peer KnownPeerCacheRecItem where
  convert peer = KnownPeerCacheRecItem
    { knownPeerCacheRecUrl = pack $ showBaseUrl $ peerUrl peer
    , knownPeerCacheRecLastValidatedAt = pack $ show $ peerLastValidatedAt peer 
    , knownPeerCacheRecIsSecureConn =  
        case peerConnScheme peer of
              Https -> True
              Http -> False
    }

instance Conversion KnownPeerCacheRecItem Peer where
  convert peer = DiscoveryTypes.Peer
    { peerUrl = fromJust $ parseBaseUrl $ unpack $ knownPeerCacheRecUrl peer
    , peerLastValidatedAt = read $ unpack $ knownPeerCacheRecLastValidatedAt peer
    , peerConnScheme = 
        case knownPeerCacheRecIsSecureConn peer of
                True  -> Https
                False -> Http
    }