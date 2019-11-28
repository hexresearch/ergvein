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
import Database.Esqueleto.Pagination

import           Conduit
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader

pageLoadSize :: PageSize
pageLoadSize = PageSize 1048576

pagedEntitiesStream ::(PersistRecordBackend record backend, PersistQueryRead backend, PersistUniqueRead backend,
                      BackendCompatible SqlBackend backend, BackendCompatible SqlBackend (BaseBackend backend),
                      Ord typ, PersistField typ, MonadIO m) 
                      => EntityField record typ -> ConduitT a (Entity record) (ReaderT backend m) ()
pagedEntitiesStream entityField = streamEntities
      emptyQuery
      entityField
      pageLoadSize
      Ascend
      (Range Nothing Nothing)

getAllTx :: (MonadIO m) => QueryT m [Entity TxRec]
getAllTx = select $ from pure

getAllTxOut :: (MonadIO m) => QueryT m [Entity TxOutRec]
getAllTxOut = select $ from pure

getAllTxIn :: (MonadIO m) => QueryT m [Entity TxInRec]
getAllTxIn = select $ from pure

getScannedHeight :: MonadIO m => Currency -> QueryT m (Maybe (Entity ScannedHeightRec))
getScannedHeight currency = fmap headMay $ select $ from $ \scannedHeight -> do
  where_ (scannedHeight ^. ScannedHeightRecCurrency ==. val currency)
  pure scannedHeight

upsertScannedHeight :: MonadIO m => Currency -> Word64 -> QueryT m (Entity ScannedHeightRec)
upsertScannedHeight currency h = upsert (ScannedHeightRec currency h) [ScannedHeightRecHeight DT.=. h]

insertTxs :: MonadIO m => [TxInfo] -> QueryT m [Key TxRec]
insertTxs txs = insertMany $ txRec <$> txs
  where 
    txRec tx = TxRec (tx'hash tx) (tx'blockHeight tx) (tx'blockIndex tx)

insertTxOuts :: MonadIO m => [TxOutInfo] -> QueryT m [Key TxOutRec]
insertTxOuts txOuts = insertMany $ txOutRec <$> txOuts
  where 
    txOutRec txOut = TxOutRec (txOut'txHash txOut) (txOut'pubKeyScriptHash txOut) (txOut'index txOut) (txOut'value txOut)

insertTxIns :: MonadIO m => [TxInInfo] -> QueryT m [Key TxInRec]
insertTxIns txIns = insertMany $ txInRec <$> txIns
  where
    txInRec txIn = TxInRec (txIn'txHash txIn) (txIn'txOutHash txIn) (txIn'txOutIndex txIn)