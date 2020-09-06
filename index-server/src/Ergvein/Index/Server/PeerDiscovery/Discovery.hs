module Ergvein.Index.Server.PeerDiscovery.Discovery 
  ( considerPeer
  , knownPeersSet
  , knownPeersActualization
  , syncWithDefaultPeers
  , ownVersion
  )where

import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Random
import Conversion
import Data.Foldable
import Data.Set (Set)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Network.Socket (SockAddr)

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.TCPService.Connections
import Ergvein.Index.Server.Utils
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Vector.Unboxed  as UV

considerPeer :: Version -> PeerCandidate -> ServerM ()
considerPeer ownVer PeerCandidate {..} = do
  ownAddress <- descReqOwnAddress <$> getDiscoveryRequisites
  isScanActual <- isPeerScanActual (versionScanBlocks ownVer) peerCandidateScanBlocks
  when (Just peerCandidateAddress /= ownAddress && isScanActual) $ do
    currentTime <- liftIO getCurrentTime
    upsertPeer $ Peer
      { peerAddress          = peerCandidateAddress
      , peerLastValidatedAt  = currentTime
      }

knownPeersSet :: [Peer] -> ServerM (Set SockAddr)
knownPeersSet discoveredPeers = do
  ownAddress <- descReqOwnAddress <$> getDiscoveryRequisites
  pure $ Set.fromList $ (toList ownAddress) ++ (peerAddress <$> discoveredPeers)

knownPeersActualization :: ServerM Thread
knownPeersActualization  = do
  create $ logOnException . scanIteration
  where
    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
     PeerDiscoveryRequisites {..} <- getDiscoveryRequisites
     currentTime <- liftIO getCurrentTime
     knownPeers <- getPeerList
     let notOutdatedPeers = isNotOutdated descReqPredefinedPeers descReqActualizationTimeout currentTime `filter` knownPeers
     setPeerList notOutdatedPeers
     openedConnectionsRef <- openConnections
     opened <- liftIO $ Map.keysSet <$> readTVarIO openedConnectionsRef
     let peersToConnect = Set.toList $ opened Set.\\ (Set.fromList $ peerAddress <$> notOutdatedPeers)
     liftIO $ forM_ peersToConnect newConnection
     broadcastSocketMessage $ MPeerRequest PeerRequest
    isNotOutdated :: Set SockAddr -> NominalDiffTime -> UTCTime -> Peer -> Bool
    isNotOutdated predefined retryTimeout currentTime peer = 
       let fromLastSuccess = currentTime `diffUTCTime` peerLastValidatedAt peer
       in Set.member (peerAddress peer) predefined || retryTimeout >= fromLastSuccess

syncWithDefaultPeers :: ServerM ()
syncWithDefaultPeers = do
  discoveredPeers <- getPeerList
  predefinedPeers <- descReqPredefinedPeers <$> getDiscoveryRequisites
  currentTime <- liftIO getCurrentTime
  let discoveredPeersSet = Set.fromList $ peerAddress <$> discoveredPeers
      toAdd = (\x -> Peer x currentTime) <$> (Set.toList $ predefinedPeers Set.\\ discoveredPeersSet)
  setPeerList toAdd

isPeerScanActual :: UV.Vector ScanBlock -> UV.Vector ScanBlock -> ServerM Bool
isPeerScanActual localScanBlocks peerScanBlocks  = do
  pure $ all matchLocalCurrencyScan peerScanBlockList
  where
    peerScanBlockList :: [ScanBlock]
    peerScanBlockList = UV.toList localScanBlocks

    matchLocalCurrencyScan :: ScanBlock -> Bool
    matchLocalCurrencyScan localBlock = 
        case peerScanInfoMap Map.!? scanBlockCurrency localBlock of
          Just peerScanBlock -> let
            filterVersionValid = scanBlockVersion localBlock == scanBlockVersion peerScanBlock
            scanValid = notLessThenOne (scanBlockScanHeight localBlock) (scanBlockScanHeight peerScanBlock)
            in filterVersionValid && scanValid
          Nothing -> False
    
    notLessThenOne :: BlockHeight -> BlockHeight -> Bool
    notLessThenOne local = (local <=) . succ

    peerScanInfoMap :: Map.Map CurrencyCode ScanBlock
    peerScanInfoMap = mapBy scanBlockCurrency peerScanBlockList

ownVersion :: ServerM Version
ownVersion = do
  nonce <- liftIO $ randomIO
  time  <- liftIO $ fromIntegral . floor <$> getPOSIXTime

  scanNfo <- UV.fromList . fmap verBlock <$> scanningInfo

  pure $ Version {
      versionVersion    = protocolVersion
    , versionTime       = time
    , versionNonce      = nonce
    , versionScanBlocks = scanNfo
    }
  where
    verBlock :: ScanProgressInfo -> ScanBlock
    verBlock ScanProgressInfo {..} = ScanBlock
     { scanBlockCurrency   = convert nfoCurrency
     , scanBlockVersion    = filterVersion nfoCurrency
     , scanBlockScanHeight = nfoScannedHeight
     , scanBlockHeight     = nfoActualHeight
     }
    
    filterVersion :: Currency -> Word32
    filterVersion = const 1