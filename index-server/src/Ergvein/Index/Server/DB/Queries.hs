module Ergvein.Index.Server.DB.Queries where

import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Database.Esqueleto
import Safe 
import qualified Database.Persist as DT

import Ergvein.Index.Server.BlockScanner.Types
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

getScannedHeight :: MonadIO m => Currency -> QueryT m (Maybe (Entity ScannedHeightRec))
getScannedHeight currency = fmap headMay $ select $ from $ \scannedHeight -> do
  where_ (scannedHeight ^. ScannedHeightRecCurrency ==. val currency)
  pure scannedHeight

upsertScannedHeight :: MonadIO m => Currency -> Word64 -> QueryT m (Entity ScannedHeightRec)
upsertScannedHeight currency h = upsert (ScannedHeightRec currency h) [ScannedHeightRecHeight DT.=. h]

insertTXOs :: MonadIO m => [TXOInfo] -> QueryT m [Key UtxoRec]
insertTXOs utxo = insertMany $ toEntity <$> utxo
  where
    toEntity u = UtxoRec (txo'txHash u) (txo'scriptHash u) (txo'outIndex u) (txo'outValue u)

insertSTXO :: MonadIO m => SpentTXOInfo -> QueryT m ()
insertSTXO stxo = insertSelect $ from $ \storedUtxo -> do
  where_ (   storedUtxo ^. UtxoRecTxHash   ==. (val $ stxo'txoHash stxo) 
         &&. storedUtxo ^. UtxoRecOutIndex ==. (val $ stxo'outIndex stxo)
         )
  return $ StxoRec <# (val $ stxo'txHash stxo) <&> (storedUtxo ^. UtxoRecId)

getAllTxo :: (MonadIO m) => QueryT m [Entity UtxoRec]
getAllTxo = select $ from pure

getAllStxo :: (MonadIO m) => QueryT m [Entity StxoRec]
getAllStxo = select $ from pure