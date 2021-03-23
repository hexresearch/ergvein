module Ergvein.Index.Server.DB.Queries
  (
    getScannedHeight
  , setScannedHeight
  , setLastScannedBlock
  , deleteLastScannedBlock
  , getLastScannedBlock
  , commitBlockInfo
  , getOutPointScript
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Parallel.Strategies
import Data.ByteString (ByteString)
import Database.RocksDB

import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Index.Server.Types
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Network.Haskoin.Transaction (OutPoint(..))

import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS

commitBlockInfo :: (HasDbs m, MonadIO m) => BlockInfo -> m ()
commitBlockInfo (BlockInfo meta spent created) = do
  db <- getDb
  ucf <- getUtxoCF cur
  fcf <- getFiltersCF cur
  mcf <- getMetaCF cur
  let heightPut = PutCF mcf scannedHeightKey heightBs
      lastScannedPut = PutCF mcf lastScannedBlockHashKey $ BSS.fromShort blkHash
      filtPut = PutCF fcf heightBs filt
      -- Tried removing the parMap and creating BatchOps without sparks. Still leaks
      -- createdPuts = mconcat $ flip fmap created $ \(th, ibs) ->
      --   flip fmap ibs $ \(i,bs) -> PutCF ucf (th <> encodeWord32 i) bs
      createdPuts = mconcat $ parMap rpar (putTx ucf) (force created)
      spentPuts = (DelCF ucf . encodeOutPoint) <$> spent
  write db $ [heightPut, lastScannedPut, filtPut] <> createdPuts <> spentPuts
  where
    BlockMeta cur height blkHash _ filt = meta
    heightBs = serializeVarInt height
    putTx ucf (th, ibs) = let
      putIbs (i, bs) = PutCF ucf (th <> encodeWord32 i) bs
      in parMap rpar putIbs (force ibs)

setLastScannedBlock :: (HasDbs m, MonadIO m) => Currency -> ShortByteString -> m ()
setLastScannedBlock currency blockHash = do
  db <- getDb
  cf <- getMetaCF currency
  putCF db cf lastScannedBlockHashKey $ BSS.fromShort blockHash

deleteLastScannedBlock :: (HasDbs m, MonadIO m) => Currency -> m ()
deleteLastScannedBlock currency = do
  db <- getDb
  cf <- getMetaCF currency
  deleteCF db cf lastScannedBlockHashKey

getLastScannedBlock :: (HasDbs m, MonadIO m) => Currency -> m (Maybe ShortByteString)
getLastScannedBlock currency = do
  db <- getDb
  cf <- getMetaCF currency
  mv <- getCF db cf lastScannedBlockHashKey
  pure $ BSS.toShort <$> mv

getScannedHeight :: (HasDbs m, MonadIO m) => Currency -> m (Maybe BlockHeight)
getScannedHeight currency = do
  db <- getDb
  cf <- getMetaCF currency
  mv <- getCF db cf scannedHeightKey
  pure $ maybe Nothing (either (const Nothing) Just . deserializeVarInt) mv

setScannedHeight :: (HasDbs m, MonadIO m) => Currency -> BlockHeight -> m ()
setScannedHeight currency height = do
  db <- getDb
  cf <- getMetaCF currency
  putCF db cf scannedHeightKey $ serializeVarInt height

-- | This one is specific for BTC
-- TODO: expand with ERGO later
getOutPointScript :: (HasDbs m, MonadIO m) => OutPoint -> m (Maybe ByteString)
getOutPointScript (OutPoint th i) = do
  db <- getDb
  cf <- getUtxoCF BTC
  getCF db cf $ (encodeBtcTxHash th <> encodeWord32 i)
