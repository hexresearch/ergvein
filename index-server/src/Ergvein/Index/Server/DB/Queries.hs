module Ergvein.Index.Server.DB.Queries where

import Data.Word
import Data.Text(Text)
import Control.Monad
import Control.Monad.IO.Class
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Database.Esqueleto
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema

import qualified Database.Persist as DT

import Safe (headMay)

data UTXOInfo = UTXOInfo {
  txHash :: TxHash,
  utxoPubKeyScriptHash :: PubKeyScriptHash,
  utxoOutIndex :: Word32,
  outValue :: MoneyUnit
} deriving Show

data STXOInfo = STXOInfo  {
  txHashTarget :: TxHash,
  stxHash :: TxHash,
  stxoOutIndex :: Word32
} deriving Show


getScannedHeight :: MonadIO m => Currency -> QueryT m (Maybe (Entity ScannedHeightRec))
getScannedHeight currency = fmap headMay $ select $ from $ \scannedHeight -> do
    where_ (scannedHeight ^. ScannedHeightRecCurrency ==. val currency)
    pure scannedHeight

updateScannedHeight :: MonadIO m => Currency -> Word64 -> QueryT m (Entity ScannedHeightRec)
updateScannedHeight currency h = upsert (ScannedHeightRec currency h) [ScannedHeightRecHeight DT.=. h]

insertUTXO :: MonadIO m => [UTXOInfo] -> QueryT m ([Key UtxoRec])
insertUTXO utxo = insertMany $ toEntity <$> utxo
  where
    toEntity u = UtxoRec (txHash u) (utxoPubKeyScriptHash u) (utxoOutIndex u) (outValue u)

insertSTXO :: MonadIO m => STXOInfo -> QueryT m ()
insertSTXO stxo = insertSelect $ from $ \storedUtxo -> do
  where_ (   storedUtxo ^. UtxoRecTxHash   ==. (val $ txHashTarget stxo) 
         &&. storedUtxo ^. UtxoRecOutIndex ==. (val $ stxoOutIndex stxo)
         )
  return $ StxoRec <# (val $ stxHash stxo) <&> (storedUtxo ^. UtxoRecId)