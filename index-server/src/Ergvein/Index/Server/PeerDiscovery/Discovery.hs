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
import Data.Foldable
import Data.Set (Set)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Data.Maybe
import Data.Text (Text)
import Network.DNS.Resolver
import Network.DNS
import Network.Socket (SockAddr)
import Foreign.C.Types (CTime(..))
import Data.Either
import Ergvein.Text

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.TCPService.Connections
import Ergvein.Index.Server.TCPService.Conversions
import Ergvein.Index.Server.Utils
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Text.Read (readMaybe)
import Data.IP

import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Vector.Unboxed  as UV
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8

considerPeer :: Version -> PeerCandidate -> ServerM ()
considerPeer ownVer PeerCandidate {..} = do
  ownAddress <- descReqOwnAddress <$> getDiscoveryRequisites
  isScanActual <- isPeerScanActual (versionScanBlocks ownVer)
  when (Just peerCandidateAddress /= ownAddress && isScanActual) $ do
    currentTime <- liftIO getCurrentTime
    upsertPeer $ let peerAddress          = peerCandidateAddress
                     peerLastValidatedAt  = currentTime
                 in Peer {..}

knownPeersSet :: [Peer] -> ServerM (Set SockAddr)
knownPeersSet discoveredPeers = do
  ownAddress <- descReqOwnAddress <$> getDiscoveryRequisites
  pure $ Set.fromList $ (toList ownAddress) ++ (peerAddress <$> discoveredPeers)

knownPeersActualization :: ServerM Thread
knownPeersActualization  = do
  create $ logOnException "knownPeersActualization" . scanIteration
  where
    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      PeerDiscoveryRequisites {..} <- getDiscoveryRequisites
      knownPeers <- getPeerList
      if length knownPeers >= 2 then do 
        currentTime <- liftIO getCurrentTime
        let upToDatePeers = isUpToDatePeer descReqPredefinedPeers descReqActualizationTimeout currentTime `filter` knownPeers
        setPeerList upToDatePeers
        openedConnectionsRef <- openConnections
        opened <- liftIO $ Map.keysSet <$> readTVarIO openedConnectionsRef
        let peersToConnect = Set.toList $ opened Set.\\ (Set.fromList $ peerAddress <$> upToDatePeers)
        liftIO $ forM_ peersToConnect newConnection
        broadcastSocketMessage $ MPeerRequest PeerRequest
      else do
        x <- liftIO initialIndexers
        pure ()
      shutdownFlagVar <- getShutdownFlag
      liftIO $ cancelableDelay shutdownFlagVar descReqActualizationDelay
      shutdownFlag <- liftIO $ readTVarIO shutdownFlagVar
      unless shutdownFlag $ scanIteration thread
    isUpToDatePeer :: Set SockAddr -> NominalDiffTime -> UTCTime -> Peer -> Bool
    isUpToDatePeer predefined retryTimeout currentTime peer =
       let fromLastSuccess = currentTime `diffUTCTime` peerLastValidatedAt peer
       in Set.member (peerAddress peer) predefined || retryTimeout >= fromLastSuccess


seedList :: [Domain]
seedList = if True
  then ["testseed.cypra.io"]
  else ["seed.cypra.io"]


defaultIndexers :: [Text]
defaultIndexers = 
  if True 
  then ["127.0.0.1:8667"]
  else ["139.59.142.25:8667", "188.244.4.78:8667"]

defaultDns :: Set.Set HostName
defaultDns = Set.fromList $ if True
  then ["8.8.8.8", "8.8.4.4", "1.1.1.1"]
  else [] -- use resolv.conf

parseSockAddrs :: (MonadIO m) => ResolvSeed -> [Text] -> m [SockAddr]
parseSockAddrs rs urls = liftIO $ do
  withResolver rs $ \resolver -> fmap catMaybes $ traverse (parseAddr resolver) urls

parseSingleSockAddr :: (MonadIO m) => ResolvSeed -> Text -> m (Maybe SockAddr)
parseSingleSockAddr rs t = do
  let (h, p) = fmap (T.drop 1) $ T.span (/= ':') t
  let port = if p == "" then defIndexerPort else fromMaybe defIndexerPort (readMaybe $ T.unpack p)
  let val = fmap (readMaybe . T.unpack) $ T.splitOn "." h
  case val of
    [Just a, Just b, Just c, Just d] -> pure $ Just $ SockAddrInet port $ tupleToHostAddress (a,b,c,d)
    _ -> do
      let url = B8.pack $ T.unpack h
      ips <- liftIO $ fmap (either (const []) id) $ withResolver rs (flip lookupA url)
      case ips of
        [] -> pure Nothing
        ip:_ -> pure $ Just $ SockAddrInet port (toHostAddress ip)

parseAddr :: Resolver -> Text -> IO (Maybe SockAddr)
parseAddr resolver t = do
  let (h, p) = fmap (T.drop 1) $ T.span (/= ':') t
  let port = if p == "" then defIndexerPort else fromMaybe defIndexerPort (readMaybe $ T.unpack p)
  let val = fmap (readMaybe . T.unpack) $ T.splitOn "." h
  case val of
    [Just a, Just b, Just c, Just d] -> pure $ Just $ SockAddrInet port $ tupleToHostAddress (a,b,c,d)
    _ -> do
      let url = B8.pack $ T.unpack h
      ips <- fmap (either (const []) id) $ lookupA resolver url
      case ips of
        [] -> pure Nothing
        ip:_ -> pure $ Just $ SockAddrInet port (toHostAddress ip)

initialIndexers :: IO [Text]
initialIndexers = do
  resolvInfo <- makeResolvSeed defaultResolvConf {
      resolvInfo = RCHostNames $ Set.toList $ defaultDns
    , resolvConcurrent = True
    }
  tryDNS <- getDNS resolvInfo seedList
  pure $ fromMaybe defaultIndexers tryDNS

syncWithDefaultPeers :: ServerM ()
syncWithDefaultPeers = do
  discoveredPeers <- getPeerList
  predefinedPeers <- descReqPredefinedPeers <$> getDiscoveryRequisites
  currentTime <- liftIO getCurrentTime
  let discoveredPeersSet = Set.fromList $ peerAddress <$> discoveredPeers
      toAdd = (`Peer` currentTime) <$> (Set.toList $ predefinedPeers Set.\\ discoveredPeersSet)
  setPeerList toAdd

isPeerScanActual :: UV.Vector ScanBlock -> ServerM Bool
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

ownVersion :: ServerM Version
ownVersion = do
  nonce <- liftIO $ randomIO
  time  <- liftIO $ CTime . floor <$> getPOSIXTime

  scanNfo <- UV.fromList <$> (mapM verBlock =<< scanningInfo)

  pure $ let versionVersion    = protocolVersion
             versionTime       = time
             versionNonce      = nonce
             versionScanBlocks = scanNfo
         in Version {..}
  where
    verBlock :: ScanProgressInfo -> ServerM ScanBlock
    verBlock ScanProgressInfo {..} = do
      currencyCode <- currencyToCurrencyCode nfoCurrency
      pure $ let scanBlockCurrency   = currencyCode
                 scanBlockVersion    = filterVersion nfoCurrency
                 scanBlockScanHeight = nfoScannedHeight
                 scanBlockHeight     = nfoActualHeight
             in ScanBlock {..}

    filterVersion :: Currency -> Word32
    filterVersion = const 1
