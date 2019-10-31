module Ergvein.Index.Server.DB.Queries where

import Data.Word
import Control.Monad
import Control.Monad.IO.Class
import Ergvein.Types.Currency
import Database.Esqueleto
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema

import qualified Database.Persist as DT

import Safe (headMay)

getScannedHeight :: MonadIO m => Currency -> QueryT m (Maybe (Entity ScannedHeightRec))
getScannedHeight currency = fmap headMay $ select $ from $ \scannedHeight -> do
    where_ (scannedHeight ^. ScannedHeightRecCurrency ==. val currency)
    pure scannedHeight

updateScannedHeight :: MonadIO m => Currency -> Word64 -> QueryT m (Entity ScannedHeightRec)
updateScannedHeight currency h = upsert (ScannedHeightRec currency h) [ScannedHeightRecHeight DT.=. h]