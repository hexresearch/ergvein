module Ergvein.Index.Server.DB.Queries where

import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Database.Esqueleto
import Safe 

import Database.Esqueleto.Pagination
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import           Conduit
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader

import qualified Data.Conduit.List as CL
import qualified Database.Persist as DT

pageLoadSize :: PageSize
pageLoadSize = PageSize 65536

pagedEntitiesStream ::(PersistRecordBackend record backend, PersistQueryRead backend, PersistUniqueRead backend,
                      BackendCompatible SqlBackend backend, BackendCompatible SqlBackend (BaseBackend backend),
                      Ord typ, PersistField typ, MonadIO m) 
                      => EntityField record typ -> ConduitT a [Entity record] (ReaderT backend m) ()
pagedEntitiesStream entityField = let
  pagedStream = streamEntities emptyQuery entityField pageLoadSize Ascend (Range Nothing Nothing)
  in pagedStream .| (CL.chunksOf $ unPageSize pageLoadSize)

getScannedHeight :: MonadIO m => Currency -> QueryT m (Maybe (Entity ScannedHeightRec))
getScannedHeight currency = fmap headMay $ select $ from $ \scannedHeight -> do
  where_ (scannedHeight ^. ScannedHeightRecCurrency ==. val currency)
  pure scannedHeight

upsertScannedHeight :: MonadIO m => Currency -> Word64 -> QueryT m (Entity ScannedHeightRec)
upsertScannedHeight currency h = upsert (ScannedHeightRec currency h) [ScannedHeightRecHeight DT.=. h]

insertTxs :: MonadIO m => [TxInfo] -> QueryT m [Key TxRec]
insertTxs txs = insertMany $ txRec <$> txs
  where 
    txRec tx = TxRec (txHash tx) (txHexView tx) (txBlockHeight tx) (txBlockIndex tx)

insertTxOuts :: MonadIO m => [TxOutInfo] -> QueryT m [Key TxOutRec]
insertTxOuts txOuts = insertMany $ txOutRec <$> txOuts
  where
    txOutRec txOut = TxOutRec (txOutTxHash txOut) (txOutPubKeyScriptHash txOut) (txOutIndex txOut) (txOutValue txOut)

insertTxIns :: MonadIO m => [TxInInfo] -> QueryT m [Key TxInRec]
insertTxIns txIns = insertMany $ txInRec <$> txIns
  where
    txInRec txIn = TxInRec (txInTxHash txIn) (txInTxOutHash txIn) (txInTxOutIndex txIn)

insertBlock :: MonadIO m => BlockMetaInfo -> QueryT m (Key BlockMetaRec)
insertBlock block = insert $ blockMetaRec block
  where
    blockMetaRec block = BlockMetaRec (blockMetaCurrency block) (blockMetaBlockHeight block) (blockMetaHeaderHexView block) (blockMetaAddressFilterHexView block)