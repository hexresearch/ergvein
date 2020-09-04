module Ergvein.Index.Server.PeerDiscovery.Discovery where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Either.Combinators
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Time.Clock
import Ergvein.Index.Server.TCPService.Connections

import Ergvein.Index.API.Types
import Ergvein.Index.Client.V1
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.TCPService.Server
import Network.Socket
import Servant.Client.Core
import qualified Data.Vector as V 
import Ergvein.Types.Currency
import qualified Data.Vector.Unboxed as UV
import Ergvein.Types.Transaction
import Conversion
import Ergvein.Types.Block

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

considerPeerCandidate1 :: PeerCandidate1 -> ExceptT PeerValidationResult ServerM ()
considerPeerCandidate1 candidate = undefined
   

knownPeersSet :: [Peer1] -> ServerM (Set SockAddr)
knownPeersSet discoveredPeers = do
  ownAddress <- descReqOwnAddress <$> getDiscoveryRequisites
  pure $ Set.fromList $ (toList ownAddress) ++ (peerAddress <$> discoveredPeers)

knownPeersActualization1 :: ServerM Thread
knownPeersActualization1 = do
  create $ logOnException . scanIteration
  where
    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
     PeerDiscoveryRequisites {..} <- getDiscoveryRequisites
     currentTime <- liftIO getCurrentTime
     knownPeers <- getKnownPeersList
     let peersToFetchFrom = isNotOutdated descReqPredefinedPeers descReqActualizationTimeout currentTime `filter` knownPeers
     setKnownPeersList peersToFetchFrom
     openedConnectionsRef <- asks envOpenConnections
     opened <- liftIO $ Map.keysSet <$> readTVarIO openedConnectionsRef
     let toOpen = Set.toList $ opened Set.\\ (Set.fromList $ peerAddress <$> peersToFetchFrom)
     liftIO $ forM_ toOpen newConnection
     broadcastSocketMessage $ MPeerRequest PeerRequest
    isNotOutdated :: Set SockAddr -> NominalDiffTime -> UTCTime -> Peer1 -> Bool
    isNotOutdated predefined retryTimeout currentTime peer = 
       let fromLastSuccess = currentTime `diffUTCTime` peerLastValidatedAt1 peer
       in Set.member (peerAddress peer) predefined || retryTimeout >= fromLastSuccess

syncWithDefaultPeers :: ServerM ()
syncWithDefaultPeers = do
  discoveredPeers <- getKnownPeersList
  predefinedPeers <- descReqPredefinedPeers <$> getDiscoveryRequisites
  currentTime <- liftIO getCurrentTime
  let discoveredPeersSet = Set.fromList $ peerAddress <$> discoveredPeers
      toAdd = (\x -> Peer1 x currentTime) <$> (Set.toList $ predefinedPeers Set.\\ discoveredPeersSet)
  addKnownPeers toAdd

isProgressMatch :: Version -> ServerM Bool
isProgressMatch ver = do
  ourProgress <- scanningInfo
  let res = all currencyScanValidation ourProgress
  pure res
  where
    currencyScanValidation :: ScanProgressInfo -> Bool
    currencyScanValidation ScanProgressInfo {..} = 
      let candidateNfo = candidateInfoMap Map.!? nfoCurrency
      in any (notLessThenOne nfoScannedHeight) candidateNfo

    notLessThenOne local = (local <=) . succ
    candidateInfoMap:: Map.Map Currency BlockHeight
    candidateInfoMap = Map.fromList $ (\x-> (convert $ scanBlockCurrency x,  scanBlockHeight x)) <$> (UV.toList $ versionScanBlocks ver)