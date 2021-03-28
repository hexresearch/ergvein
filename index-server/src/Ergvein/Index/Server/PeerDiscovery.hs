module Ergvein.Index.Server.PeerDiscovery
  ( considerPeer
  , knownPeersSet
  , knownPeersActualization
  , syncWithDefaultPeers
  , ownVersion
  , newConnection
  )where

import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Catch
import Control.Monad.Random
import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.Socket (SockAddr, HostName, ServiceName, NameInfoFlag(..), getNameInfo)
import Foreign.C.Types (CTime(..))

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.Types
import Ergvein.Index.Server.DB.Queries.Peers
import Ergvein.Index.Server.Scanner
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Conversions
import Ergvein.Index.Server.Utils
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Vector.Unboxed  as UV
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Bitcoin.API

considerPeer :: ServerMonad m => Version -> PeerCandidate -> m ()
considerPeer ownVer PeerCandidate {..} = do
  ownAddress <- descReqOwnAddress <$> getDiscoveryRequisites
  isScanActual <- isPeerScanActual (versionScanBlocks ownVer)
  when (Just peerCandidateAddress /= ownAddress && isScanActual) $ do
    currentTime <- liftIO getCurrentTime
    insertPeer $ Peer
      { peerAddress          = peerCandidateAddress
      , peerLastValidatedAt  = currentTime
      }

knownPeersSet :: ServerMonad m => [Peer] -> m (Set SockAddr)
knownPeersSet discoveredPeers = do
  ownAddress <- descReqOwnAddress <$> getDiscoveryRequisites
  pure $ Set.fromList $ (toList ownAddress) ++ (peerAddress <$> discoveredPeers)

knownPeersActualization :: ServerMonad m => m Thread
knownPeersActualization  = create $ logOnException threadName . threadBody
  where
    threadName = "knownPeersActualization"
    threadBody :: ServerMonad m => Thread -> m ()
    threadBody thread = do
      shutdownChan <- getShutdownChannel
      b <- liftIO $ atomically $ readTChan shutdownChan
      liftIO $ when b $ stop thread

syncWithDefaultPeers :: ServerMonad m => m ()
syncWithDefaultPeers = do
  discoveredPeers <- getPeerList
  predefinedPeers <- descReqPredefinedPeers <$> getDiscoveryRequisites
  currentTime <- liftIO getCurrentTime
  let discoveredPeersSet = Set.fromList $ peerAddress <$> discoveredPeers
      toAdd = (\x -> Peer x currentTime) <$> (Set.toList $ predefinedPeers Set.\\ discoveredPeersSet)
  setPeerList toAdd

isPeerScanActual :: ServerMonad m => UV.Vector ScanBlock -> m Bool
isPeerScanActual localScanBlocks = do
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

newConnection :: SockAddr -> IO (HostName, ServiceName)
newConnection addr = do
  (maybeHost, maybePort) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True addr
  pure (fromJust maybeHost , fromJust maybePort)

ownVersion :: (MonadIO m, HasDbs m, BitcoinApiMonad m, HasServerConfig m, MonadCatch m) => m Version
ownVersion = do
  nonce <- liftIO $ randomIO
  time  <- liftIO $ CTime . floor <$> getPOSIXTime

  scanNfo <- UV.fromList <$> (mapM verBlock =<< scanningInfo)

  pure $ Version {
      versionVersion    = protocolVersion
    , versionTime       = time
    , versionNonce      = nonce
    , versionScanBlocks = scanNfo
    }
  where
    verBlock :: (MonadIO m, HasServerConfig m) => ScanProgressInfo -> m ScanBlock
    verBlock ScanProgressInfo {..} = do
      currencyCode <- currencyToCurrencyCode nfoCurrency
      pure $ ScanBlock
        { scanBlockCurrency   = currencyCode
        , scanBlockVersion    = filterVersion nfoCurrency
        , scanBlockScanHeight = nfoScannedHeight
        , scanBlockHeight     = nfoActualHeight
        }

    filterVersion :: Currency -> ProtocolVersion
    filterVersion = const (1, 0, 0)
