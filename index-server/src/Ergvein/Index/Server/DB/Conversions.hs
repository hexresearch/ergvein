module Ergvein.Index.Server.DB.Conversions where

import Conversion
import Data.Text
import Data.Maybe
import Servant.Client.Core
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer (KnownPeerRecItem(..))
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

instance Conversion KnownPeerRecItem DiscoveryTypes.Peer where
  convert peer = DiscoveryTypes.Peer
    { peerUrl = fromJust $ parseBaseUrl $ unpack $ knownPeerRecUrl peer
    , peerLastValidatedAt = read $ unpack $ knownPeerRecLastValidatedAt peer
    , peerConnScheme =
        case knownPeerRecIsSecureConn peer of
                True  -> Https
                False -> Http
    }
