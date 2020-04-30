module Ergvein.Index.Server.DB.Schema where

import Data.Text
import Data.Time
import Database.Persist.TH
import Database.Persist.Types
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Drv
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Servant.Client.Core

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
  UniqueCurrencyHeight currency height
  deriving Show
DiscoveredPeer
  url Text
  lastValidatedAt UTCTime
  isSecureConnection Bool
  UniqueUrl url
  |]