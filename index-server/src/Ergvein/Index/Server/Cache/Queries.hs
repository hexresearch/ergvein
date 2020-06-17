module Ergvein.Index.Server.Cache.Queries where

import Control.Lens
import Data.Default
import Data.Flat
import Data.Maybe
import Database.LevelDB
import Database.LevelDB.Iterator
import Control.Monad.Logger
import Conversion
import Data.Time.Clock
import Control.Monad.IO.Class

import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Types.Transaction
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Cache.Conversions
import Ergvein.Index.Server.PeerDiscovery.Types

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Map.Strict as Map
import qualified Database.LevelDB as LDB
import qualified Database.LevelDB.Streaming as LDBStreaming

safeEntrySlice :: (MonadLDB m , Ord k, S.Serialize k, Flat v) => BS.ByteString -> k -> m [(k,v)]
safeEntrySlice startKey endKey = do
  db <- getDb
  iterator <- createIter db def
  slice <- LDBStreaming.toList $ LDBStreaming.entrySlice iterator range LDBStreaming.Asc
  pure $ over _2 unflatExact  <$> over _1 (decodeExact . unPrefixedKey) <$> slice
  where
    range = LDBStreaming.KeyRange startKey comparison
    comparison key = case S.decode $ unPrefixedKey key of
      Right parsedKey -> compare parsedKey endKey
      _ -> GT

getParsed :: (MonadLDB m, Flat v) => BS.ByteString -> m (Maybe v)
getParsed key = do
  db <- getDb
  maybeResult <- get db def key
  let maybeParsedResult = unflatExact <$> maybeResult
  pure maybeParsedResult

getParsedExact :: (MonadLDB m, MonadLogger m, Flat v) => BS.ByteString -> m v
getParsedExact key = do
  db <- getDb
  maybeResult <- get db def key
  case maybeResult of
    Just result -> pure $ unflatExact result
    Nothing -> do
      currentTime <- liftIO getCurrentTime
      logErrorN $ "[Cache miss][getParsedExact] Entity with key " <> (T.pack $ show key) <> " not found at time:" <> (T.pack $ show currentTime)
      error $ "getParsedExact: not found" ++ show key

getManyParsedExact :: (MonadLDB m, MonadLogger m, Flat v) => [BS.ByteString] -> m [v]
getManyParsedExact keys = do
  db <- getDb
  result <- mapM getParsedExact keys
  pure result

putItems :: (Flat v) => (a -> BS.ByteString) -> (a -> v) -> [a] -> LDB.WriteBatch
putItems keySelector valueSelector items = putI <$> items
  where putI item = LDB.Put (keySelector item) $ flat $ valueSelector item

updateTxSpends  :: (MonadLDB m, MonadLogger m) => [TxHash] -> [TxInfo] -> m ()
updateTxSpends spentTxsHash newTxInfos = do
  db <- getDb
  write db def $ putItems (cachedTxKey . txHash) (convert @_ @TxCacheRec) newTxInfos
  cachedInfo <- getManyParsedExact @_ @TxCacheRec  $ cachedTxKey <$> Map.keys outSpendsAmountByTx 
  write db def $ infoUpdate <$> cachedInfo
  where
    outSpendsAmountByTx = Map.fromListWith (+) $ (,1) <$> spentTxsHash
    infoUpdate info = let 
      outputsLeft = txCacheRecUnspentOutputsCount info - outSpendsAmountByTx Map.! (txCacheRecHash info)
      in if outputsLeft == 0 then 
          LDB.Del $ cachedTxKey $ txCacheRecHash info
         else
          LDB.Put (cachedTxKey $ txCacheRecHash info) (flat $ info { txCacheRecUnspentOutputsCount = outputsLeft })

updateKnownPeers :: (MonadLDB m) => [Peer] -> m ()
updateKnownPeers peers = do
  db <- getDb
  put db def cachedKnownPeersKey $ flat $ KnownPeersCacheRec $ convert <$> peers

getKnownPeers :: (MonadLDB m, MonadLogger m) => Bool -> m [String]
getKnownPeers onlySecured = do
  db <- getDb
  knownPeers <- getParsedExact cachedKnownPeersKey
  pure $ T.unpack . knownPeerCacheRecUrl <$>
    if onlySecured then filter knownPeerCacheRecIsSecureConn knownPeers else knownPeers