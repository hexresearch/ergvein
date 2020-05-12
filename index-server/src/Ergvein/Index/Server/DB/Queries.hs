module Ergvein.Index.Server.DB.Queries where

import Conduit
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Conversion
import Data.Proxy
import Data.Time.Clock
import Data.Word
import Database.Esqueleto
import Database.Esqueleto.Pagination
import Safe

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Conversions
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Conduit.Internal as DCI
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

addNewPeers :: MonadIO m => [NewPeer] -> QueryT m ()
addNewPeers newPeers = do
  currentTime <- liftIO getCurrentTime
  forM_ newPeers (insert_ . convert @_ @DiscoveredPeerRec . (currentTime, ))

refreshPeerValidationTime :: MonadIO m => [DiscoveredPeerRecId] -> QueryT m ()
refreshPeerValidationTime peerIds = do
  currentTime <- liftIO getCurrentTime
  update $ \peer -> do 
    where_ $ peer ^. DiscoveredPeerRecId `in_` valList peerIds
    set peer [DiscoveredPeerRecLastValidatedAt =. val currentTime]

deleteExpiredPeers :: MonadIO m => [DiscoveredPeerRecId] -> QueryT m ()
deleteExpiredPeers peerIds =
  delete $ from $ \peer -> 
    where_ $ peer ^. DiscoveredPeerRecId `in_` valList peerIds

getDiscoveredPeers :: MonadIO m => Bool -> QueryT m [Peer]
getDiscoveredPeers onlySecured = do
  result <- select $ from $ \peer -> do
    when onlySecured $ 
      where_ $ peer ^. DiscoveredPeerRecIsSecureConnection ==. val onlySecured
    pure peer
  pure $ convert @(Entity DiscoveredPeerRec) <$> result


insertBlock  :: MonadIO m  => BlockMetaInfo -> QueryT m (Key BlockMetaRec)
insertBlock block = insert $ convert block

isNonePeersDiscovered :: MonadIO m  => QueryT m Bool
isNonePeersDiscovered = (0 == ) <$> rowsCount (Proxy :: Proxy DiscoveredPeerRec)

rowsCount :: forall record m . (BackendCompatible SqlBackend (PersistEntityBackend record),
                                PersistEntity record, MonadIO m)
                                 => Proxy record -> QueryT m Word64
rowsCount _ = do 
  result <- select $ from (\(_ :: SqlExpr (Entity record)) -> pure $ countRows)
  pure $ unValue $ head $ result

chunksCount :: forall record m . (BackendCompatible SqlBackend (PersistEntityBackend record),
                                PersistEntity record, MonadIO m)
                                 => Proxy record -> QueryT m Word64
chunksCount _ = do 
  rCount <- rowsCount (Proxy :: Proxy record)
  let cCount = ceiling $ fromIntegral rCount / (fromIntegral $ unPageSize pageLoadSize)
  pure cCount