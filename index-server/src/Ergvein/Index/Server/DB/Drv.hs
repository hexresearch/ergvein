module Ergvein.Index.Server.DB.Drv where

import Database.Persist.TH
import Ergvein.Types.Currency

derivePersistField "Currency"