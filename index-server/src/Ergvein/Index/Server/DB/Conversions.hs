module Ergvein.Index.Server.DB.Conversions where

import Conversion
import Database.Persist.Types
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.BlockchainScanning.Types

instance Conversion (Entity BlockMetaRec) BlockMetaInfo where
  convert entity = let 
    value = entityVal entity 
    in BlockMetaInfo  (blockMetaRecCurrency value) 
                      (blockMetaRecHeight value)
                      (blockMetaRecBlockHeaderHashHexView value) 
                      (blockMetaRecAddressFilterHexView value)