module Ergvein.Index.Server.DB.Schema where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
        ScannedHeightRec
        
    |]