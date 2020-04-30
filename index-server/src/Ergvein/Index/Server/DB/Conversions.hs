module Ergvein.Index.Server.DB.Conversions where

import Conversion
import Database.Persist.Types
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.PeerDiscovery.Types
import Servant.Client.Core
import qualified Data.Text as T

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

instance Conversion DiscoveredPeer DiscoveredPeerRec where
  convert discoveredPeer = DiscoveredPeerRec
    { discoveredPeerRecUrl = T.pack $ showBaseUrl $ discPeerUrl discoveredPeer
    , discoveredPeerRecLastValidatedAt = discPeerLastValidatedAt discoveredPeer
    , discoveredPeerRecIsSecureConnection = 
        case discPeerConnectionScheme discoveredPeer of
          Https -> True
          Http  -> False
    }