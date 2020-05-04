module Ergvein.Index.Server.DB.Conversions where

import Conversion
import Database.Persist.Types
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.PeerDiscovery.Types
import Servant.Client.Core
import qualified Data.Text as T
import Data.Maybe
import Data.Time.Clock

instance Conversion (Entity BlockMetaRec) BlockMetaInfo where
  convert entity = let 
    value = entityVal entity 
    in BlockMetaInfo
      { blockMetaCurrency = blockMetaRecCurrency value
      , blockMetaBlockHeight = blockMetaRecHeight value
      , blockMetaHeaderHashHexView = blockMetaRecBlockHeaderHashHexView value
      , blockMetaAddressFilterHexView = blockMetaRecAddressFilterHexView value
      }

instance Conversion BlockMetaInfo BlockMetaRec where
  convert block = BlockMetaRec
    { blockMetaRecCurrency = blockMetaCurrency block 
    , blockMetaRecHeight = blockMetaBlockHeight block
    , blockMetaRecBlockHeaderHashHexView = blockMetaHeaderHashHexView block
    , blockMetaRecAddressFilterHexView = blockMetaAddressFilterHexView block
    } 

instance Conversion (UTCTime , NewPeer) DiscoveredPeerRec where
  convert (t,discoveredPeer) = DiscoveredPeerRec
    { discoveredPeerRecUrl = T.pack $ showBaseUrl $ newPeerUrl discoveredPeer
    , discoveredPeerRecLastValidatedAt = t
    , discoveredPeerRecIsSecureConnection = 
        case newPeerConnectionScheme discoveredPeer of
          Https -> True
          Http  -> False
    }

instance Conversion (Entity DiscoveredPeerRec) Peer where
  convert entity = let
    key = entityKey entity
    value = entityVal entity
    in Peer
    { peerId = key
    , peerUrl = fromJust $ parseBaseUrl $ T.unpack $ discoveredPeerRecUrl value
    , peerLastValidatedAt = discoveredPeerRecLastValidatedAt value
    , peerConnectionScheme =
        case discoveredPeerRecIsSecureConnection value of
            True  -> Https
            False -> Http
    }