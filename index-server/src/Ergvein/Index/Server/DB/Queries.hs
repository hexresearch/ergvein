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

data Unspent = Unspent {
  txHash :: String,
  pubKey :: String,
  amount :: MoneyUnit
} deriving Show

data Spent = Spent  {
  txHashTarget :: String,
  stxHash :: String,
  spubKey :: String
} deriving Show


getScannedHeight :: MonadIO m => Currency -> QueryT m (Maybe (Entity ScannedHeightRec))
getScannedHeight currency = fmap headMay $ select $ from $ \scannedHeight -> do
    where_ (scannedHeight ^. ScannedHeightRecCurrency ==. val currency)
    pure scannedHeight

updateScannedHeight :: MonadIO m => Currency -> Word64 -> QueryT m (Entity ScannedHeightRec)
updateScannedHeight currency h = upsert (ScannedHeightRec currency h) [ScannedHeightRecHeight DT.=. h]

insertUTXO :: MonadIO m => Unspent -> QueryT m (Key UtxoRec)
insertUTXO utxo = insert (UtxoRec (txHash utxo) (pubKey utxo) (amount utxo))