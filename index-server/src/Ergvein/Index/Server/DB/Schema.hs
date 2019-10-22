module Ergvein.Index.Server.DB.Schema where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word
import Database.Persist.TH
import Ergvein.Types.Currency
import Ergvein.Index.Server.DB.Drv



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ScannedHeightRec
  currency Currency
  height Word64
  UniqueCurrency currency
  |]