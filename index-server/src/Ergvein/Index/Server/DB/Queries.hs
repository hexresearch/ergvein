module Ergvein.Index.Server.DB.Queries
  (
    getScannedHeight
  , setScannedHeight
  , storeLastScannedBlock
  , deleteLastScannedBlock
  , loadLastScannedBlock
  , commitBlockInfo
  , getOutPointScript
  , performRollback
  ) where

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens.Combinators (none)
import Control.Monad.IO.Class
import Control.Parallel.Strategies
import Data.ByteString (ByteString)
import Data.Word
import Database.RocksDB
import Network.Haskoin.Script (isDataCarrier, decodeOutputBS)

import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Index.Server.Types
import Ergvein.Index.Server.Monad.Class
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Network.Haskoin.Transaction (OutPoint(..))

import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

commitBlockInfo :: (HasDbs m, HasBtcCache m, LastScannedBlockStore m) => BlockInfo -> m ()
commitBlockInfo (BlockInfo meta spent created) = do
  db <- getDb
  ucf <- getUtxoCF cur
  fcf <- getFiltersCF cur
  let filtPut = PutCF fcf heightBs filt
      (createdIds, createdPuts) = fmap mconcat $ unzip $ parMap rpar (putTx ucf) (force created)
  write db $ [filtPut] <> createdPuts
  setLastScannedBlock cur $ Just blkHash
  insertCacheEntry cur $ CacheEntry height blkHash spent createdIds
  where
    BlockMeta cur height blkHash _ filt = meta
    heightBs = encodeWord64 height
    putTx ucf (th, ibs) = let
      ibsl :: Word32 = fromIntegral $ length ibs
      putIbs (i, bs) = PutCF ucf (th <> encodeWord32 i) bs
      in ((th, ibsl), ) $ parMap rpar putIbs (removeDataCarriers $ force ibs)

    removeDataCarriers :: [(Word32, ByteString)] -> [(Word32, ByteString)]
    removeDataCarriers = case cur of
      BTC -> filter (none isDataCarrier . decodeOutputBS . snd)
      _ -> id
    {-# INLINE removeDataCarriers #-}

storeLastScannedBlock :: HasDbs m => Currency -> ShortByteString -> m ()
storeLastScannedBlock currency blockHash = do
  db <- getDb
  cf <- getMetaCF currency
  putCF db cf lastScannedBlockHashKey $ BSS.fromShort blockHash

deleteLastScannedBlock :: HasDbs m => Currency -> m ()
deleteLastScannedBlock currency = do
  db <- getDb
  cf <- getMetaCF currency
  deleteCF db cf lastScannedBlockHashKey

loadLastScannedBlock :: HasDbs m => Currency -> m (Maybe ShortByteString)
loadLastScannedBlock currency = do
  db <- getDb
  cf <- getMetaCF currency
  mv <- getCF db cf lastScannedBlockHashKey
  pure $ BSS.toShort <$> mv

getScannedHeight :: HasDbs m => Currency -> m (Maybe BlockHeight)
getScannedHeight currency = do
  db <- getDb
  cf <- getMetaCF currency
  mv <- getCF db cf scannedHeightKey
  pure $ maybe Nothing (either (const Nothing) Just . decodeWord64) mv

setScannedHeight :: HasDbs m => Currency -> BlockHeight -> m ()
setScannedHeight currency height = do
  db <- getDb
  cf <- getMetaCF currency
  putCF db cf scannedHeightKey $ encodeWord64 height

-- | This one is specific for BTC
-- TODO: expand with ERGO later
getOutPointScript :: HasDbs m => OutPoint -> m (Maybe ByteString)
getOutPointScript (OutPoint th i) = do
  db <- getDb
  cf <- getUtxoCF BTC
  getCF db cf $ (encodeBtcTxHash th <> encodeWord32 i)

insertCacheEntry :: (HasDbs m, HasBtcCache m, MonadIO m) => Currency -> CacheEntry -> m ()
insertCacheEntry cur entry = do
  cvar <- getBtcCacheVar
  cache <- liftIO $ readTVarIO cvar
  if Seq.length cache < 64
    then liftIO $ atomically $ writeTVar cvar $ entry Seq.<| cache
    else case Seq.viewr cache of
      Seq.EmptyR -> pure () -- wut?
      rest Seq.:> (CacheEntry height blkHash spent _) -> do
        db <- getDb
        ucf <- getUtxoCF cur
        mcf <- getMetaCF cur
        let heightPut = PutCF mcf scannedHeightKey $ encodeWord64 height
            lastScannedPut = PutCF mcf lastScannedBlockHashKey $ BSS.fromShort blkHash
            spentPuts = (DelCF ucf . encodeOutPoint) <$> spent
        write db $ [heightPut, lastScannedPut] <> spentPuts
        liftIO $ atomically $ writeTVar cvar $ entry Seq.<| rest

performRollback :: (HasDbs m, HasBtcCache m, MonadIO m) => Currency -> m Int
performRollback cur = do
  cvar <- getBtcCacheVar
  cache <- liftIO $ readTVarIO cvar
  db <- getDb
  ucf <- getUtxoCF cur
  let dels = mconcat $ flip fmap (Foldable.toList cache) $ \(CacheEntry _ _ _ created) ->
        mconcat $ flip fmap created $ \(th, i) ->
          (DelCF ucf . (<>) th . encodeWord32) <$> [0..i]
  write db dels
  liftIO $ atomically $ writeTVar cvar mempty
  pure $ Seq.length cache
