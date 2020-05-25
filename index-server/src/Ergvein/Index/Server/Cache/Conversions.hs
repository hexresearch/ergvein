module Ergvein.Index.Server.Cache.Conversions where

import Conversion
import Data.Text
import Servant.Client.Core
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Index.Server.PeerDiscovery.Types

instance Conversion TxInfo TxCacheRec where
  convert txInfo = TxCacheRec (txHash txInfo) (txHexView txInfo) (txOutputsCount txInfo)

instance Conversion Peer KnownPeerCacheRec where
  convert peer = KnownPeerCacheRec
    { knownPeerCacheRecUrl = pack $ showBaseUrl $ peerUrl peer
    , knownPeerCacheRecIsSecureConn =  
        case peerConnScheme peer of
              Https -> True
              Http -> False
    }