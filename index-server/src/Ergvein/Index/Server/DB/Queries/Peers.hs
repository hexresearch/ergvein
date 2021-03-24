module Ergvein.Index.Server.DB.Queries.Peers
  (
    getPeerList
  , getActualPeers
  , setPeerList
  , insertPeer
  , deletePeerBySockAddr
  , emptyKnownPeers
  ) where

import Control.Monad.IO.Class
import Data.Either
import Data.Time
import Database.RocksDB as RDB
import Network.Socket

import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.Monad.Class
import Ergvein.Index.Server.Types

import qualified Data.Persist as P

getPeerList :: HasDbs m => m [Peer]
getPeerList = do
  db <- getDb
  mv <- RDB.get db knownPeersKey
  pure $ maybe [] (fromRight [] . P.decode) mv

getActualPeers :: (HasDbs m, HasDiscoveryRequisites m) => m [Peer]
getActualPeers = do
  currentTime <- liftIO getCurrentTime
  actualizationDelay <- (/1000000) . fromIntegral . descReqActualizationDelay <$> getDiscoveryRequisites
  knownPeers <- getPeerList
  let validDate = (-actualizationDelay) `addUTCTime` currentTime
  pure $ filter ((validDate <=) . peerLastValidatedAt) knownPeers

setPeerList :: HasDbs m => [Peer] -> m ()
setPeerList peers = do
  db <- getDb
  RDB.put db knownPeersKey $ P.encode peers

insertPeer :: HasDbs m => Peer -> m ()
insertPeer peer = do
  peers <- getPeerList
  setPeerList $ peer : (excludePeerByAddr (peerAddress peer) peers)

deletePeerBySockAddr :: HasDbs m => SockAddr -> m ()
deletePeerBySockAddr addr = setPeerList . excludePeerByAddr addr =<< getPeerList

emptyKnownPeers :: HasDbs m => m ()
emptyKnownPeers = setPeerList []
{-# INLINE emptyKnownPeers #-}

excludePeerByAddr :: SockAddr ->  [Peer] -> [Peer]
excludePeerByAddr addr = filter ((== addr) . peerAddress)
{-# INLINE excludePeerByAddr #-}
