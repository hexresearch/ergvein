module Ergvein.Index.Server.DB.Schema where

import Database.Persist.TH
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block
import Ergvein.Index.Server.DB.Drv
import Ergvein.Index.Server.BlockchainScanning.Types
import Conversion
import Database.Persist.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ScannedHeightRec
  currency Currency
  height BlockHeight
  UniqueCurrency currency
  deriving Show
BlockMetaRec
  currency Currency
  height BlockHeight
  blockHeaderHashHexView BlockHeaderHashHexView
  addressFilterHexView AddressFilterHexView
  deriving Show
  |]

instance Conversion (Entity BlockMetaRec) BlockMetaInfo where
  convert entity = let 
    value = entityVal entity 
    in BlockMetaInfo (blockMetaRecCurrency value) (blockMetaRecHeight value) (blockMetaRecBlockHeaderHashHexView value) (blockMetaRecAddressFilterHexView value)