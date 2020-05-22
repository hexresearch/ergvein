module Ergvein.Index.Server.Server.V1 where

import Data.Flat
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Word
import Database.Persist.Sql

import Ergvein.Index.API
import Ergvein.Index.API.Types
import Ergvein.Index.API.V1
import Ergvein.Index.Server.Cache.Queries
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Monad
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.Dependencies

import Data.Proxy
import Servant.API
import Servant.API.Generic
import Servant.Client

import qualified Network.Haskoin.Block as Btc
import qualified Data.Serialize as S 
import Ergvein.Index.Server.PeerDiscovery.Discovery
import Ergvein.Index.Server.PeerDiscovery.Types
import Debug.Trace
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Except
import Data.Time.Clock

indexServer :: IndexApi AsServerM
indexServer = IndexApi
    { indexGetHeight = indexGetHeightEndpoint
    , indexGetBlockFilters = indexGetBlockFiltersEndpoint
    , indexGetInfo = indexGetInfoEndpoint
    , indexIntroducePeer = introducePeerEndpoint
    , indexKnownPeers = knownPeersEndpoint
    }

--Endpoints
indexGetHeightEndpoint :: HeightRequest -> ServerM HeightResponse
indexGetHeightEndpoint (HeightRequest currency) = do
  mh <- dbQuery $ fmap (scannedHeightRecHeight . entityVal) <$> getScannedHeight currency
  pure $ HeightResponse $ fromMaybe 0 mh

getBlockMetaSlice :: Currency -> BlockHeight -> BlockHeight -> ServerM [BlockMetaCacheRec]
getBlockMetaSlice currency startHeight endHeight = do
  let start = cachedMetaKey (currency, startHeight) 
      end   = BlockMetaCacheRecKey currency $ startHeight + pred endHeight
  slice <- safeEntrySlice start end
  let metaSlice = snd <$> slice
  pure metaSlice

indexGetBlockFiltersEndpoint :: BlockFiltersRequest -> ServerM BlockFiltersResponse
indexGetBlockFiltersEndpoint request = do
    slice <- getBlockMetaSlice (filtersReqCurrency request) (filtersReqStartHeight request) (filtersReqAmount request)
    let blockFilters = (\s -> (blockMetaCacheRecHeaderHashHexView s, blockMetaCacheRecAddressFilterHexView s)) <$> slice
    pure blockFilters

indexGetInfoEndpoint :: ServerM InfoResponse
indexGetInfoEndpoint = do 
  scanInfo <- scanningInfo
  let mappedScanInfo = scanNfoItem <$> scanInfo
  pure $ InfoResponse mappedScanInfo
  where
    scanNfoItem nfo = ScanProgressItem (nfoCurrency nfo) (nfoScannedHeight nfo) (nfoActualHeight nfo)

introducePeerEndpoint :: IntroducePeerReq -> ServerM IntroducePeerResp
introducePeerEndpoint request = do
  url <- PeerCandidate <$> (parseBaseUrl $ introducePeerReqUrl $ request) 
  result <- runExceptT $ considerPeerCandidate url
  pure $ peerValidationToResponce result

peerValidationToResponce :: Either PeerValidationResult () -> IntroducePeerResp
peerValidationToResponce = \case 
  Right ()   -> IntroducePeerResp True Nothing
  Left error -> IntroducePeerResp False $ Just $ case error of
    AlreadyKnown ->
      "Peer with such address already known"
    InfoEndpointError ->
      "Unable to establish connection to Info endpoint"
    CurrencyOutOfSync outOfSync -> 
      "Currency " <> show (outOfsyncCurrency outOfSync) <> "scanned height much less then " <> show (outOfSyncLocalHeight outOfSync)
    CurrencyMissing currency ->
      "Currency " <> show currency <> "is missing"
    KnownPeersEndpointError ->
      "Unable to establish connection to knownPeers endpoint"


knownPeersEndpoint :: KnownPeersReq -> ServerM KnownPeersResp
knownPeersEndpoint request = do 
  result <- getKnownPeers $ knownPeersWithSecuredOnly request
  pure $ KnownPeersResp result