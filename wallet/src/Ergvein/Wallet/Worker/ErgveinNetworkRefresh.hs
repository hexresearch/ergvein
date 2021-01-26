{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Ergvein.Wallet.Worker.ErgveinNetworkRefresh
  ( ergveinNetworkRefresh
  , ensureErgveinNetwork
  ) where

import Control.Lens
import Control.Lens.Fold
import Control.Monad.IO.Class
import Data.Either.Combinators
import Data.Map.Lens
import Data.Maybe
import Data.Time.Clock
import Network.DNS
import Network.Socket
import Reflex.ExternalRef
import System.Random.Shuffle

import Ergvein.Index.Protocol.Types
import Ergvein.Text
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Util
import Ergvein.Wallet.Platform
import Ergvein.DNS.Crawling

import qualified Data.Attoparsec.Binary     as P
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.Vector                as V
import qualified Ergvein.Types.Currency     as C

operableNetworkSize, targetNetworkSize :: Int
operableNetworkSize | isTestnet = 1
                    | otherwise = 2
targetNetworkSize = 16

workerDelay :: NominalDiffTime
workerDelay = 4

ergveinNetworkRefresh ::(MonadFront t m, MonadIO m, MonadIndexClient t m, MonadHasSettings t m, PlatformNatives) => m ()
ergveinNetworkRefresh = do
  refreshTimerE <- void <$> tickLossyFromPostBuildTime workerDelay
  buildE <- getPostBuild
  isDiscoveryEnabledD <- fmap _settingsDiscoveryEnabled <$> getSettingsD
  activeConnectionsRef <- getActiveConnsRef
  let refreshE = gate (current isDiscoveryEnabledD) $ leftmost [refreshTimerE, buildE]
  offlinePeersRemoveE <- performEvent $ ffor refreshE $ \_-> do
    activeConnections <- readExternalRef activeConnectionsRef
    pure $ settingsErgveinNetwork %~ Map.filterWithKey (\address info -> _nfoIsUserModified info || (activeConnections & has (at address . _Just)))

  offlinePeersRemovedE <- updateSettingsAsync offlinePeersRemoveE
  activeConnectionsE <- performEvent $ ffor offlinePeersRemovedE $ const $ readExternalRef activeConnectionsRef

  let activePeerAmountE  = length . Map.toList <$> activeConnectionsE
      notOperablePeerAmountE  = ffilter (< operableNetworkSize) activePeerAmountE 
      insufficientPeerAmountE = void $ ffilter (< targetNetworkSize) activePeerAmountE
  restoreNetwork notOperablePeerAmountE
  fetchNewPeer insufficientPeerAmountE

ensureErgveinNetwork :: (MonadIndexClient t m, MonadHasSettings t m) => m ()
ensureErgveinNetwork  = do
  settingsRef <- getSettingsRef
  buildE <- getPostBuild
  activeUrlsE <- performEvent $ ffor buildE $ const $ readExternalRef settingsRef
  let activeUrlsE' = ffilter (== 0) $ length . _settingsErgveinNetwork <$> activeUrlsE
  restoreNetwork activeUrlsE'

restoreNetwork :: (MonadIndexClient t m, MonadHasSettings t m) => Event t Int -> m ()
restoreNetwork notOperablePeerAmountE = do
  dnsSettingsD <- fmap _settingsDns <$> getSettingsD
  reloadedFromSeedE <- performEvent $ ffor notOperablePeerAmountE $ \presentAmount -> do
    dns <- sampleDyn dnsSettingsD
    rs <- liftIO $ resolveSeed $ Set.toList dns 
    newSet <- liftIO $ getDNS rs defErgveinNodeSeedList
    neededSet <- take (targetNetworkSize - presentAmount) <$> (liftIO $ shuffleM $ fromMaybe defaultErgveinNodeAddresses newSet)
    pure neededSet
  void $ addManyDiscovered reloadedFromSeedE

fetchNewPeer :: MonadFront t m => Event t () -> m ()
fetchNewPeer e = do
  let reqE = (C.BTC, MPeerRequest PeerRequest) <$ e
  respE <- requestRandomIndexer reqE
  let nonEmptyAddressesE = fforMaybe respE $ \(_, msg) -> case msg of
        MPeerResponse PeerResponse {..} | not $ V.null peerResponseAddresses -> Just peerResponseAddresses
        _-> Nothing
  newAddressE <- performEvent $ ffor nonEmptyAddressesE $ liftIO . fmap head . shuffleM . V.toList
  let (parsingErrorE, parsedAddressE) = fanEither $ convertAddress <$> newAddressE
  
  logInfo $ showt . ("[ErgveinNetworkRefresh] failed to decode address, error:" <>) <$> parsingErrorE
  void $ addDiscovered parsedAddressE
  where
    convertAddress :: Address -> Either String Text 
    convertAddress Address{..} = case addressType of
      IPV4 -> let
        port = (fromInteger $ toInteger addressPort)
        in (\ip -> showt $ SockAddrInet port ip) <$> P.parseOnly parseV4 addressAddress
      IPV6 -> let
        port = (fromInteger $ toInteger addressPort)
        in (\ip -> showt $ SockAddrInet6 port 0 ip 0) <$> P.parseOnly parseV6 addressAddress
      where 
        parseV4 = P.anyWord32be
        parseV6 = (,,,) <$> P.anyWord32be <*> P.anyWord32be <*> P.anyWord32be <*> P.anyWord32be