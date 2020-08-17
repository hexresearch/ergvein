module Ergvein.Index.Server.Server.V1 where

import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Flat
import Data.List
import Data.Map.Strict (Map, restrictKeys)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Time.Clock
import Data.Word
import Servant.API
import Servant.API.Generic
import Servant.Client

import Ergvein.Index.API
import Ergvein.Index.API.Types
import Ergvein.Index.API.V1
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Discovery
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Transaction

import qualified Ergvein.Index.Protocol.Types as IPT
import qualified Data.Map.Strict as M
import qualified Data.Serialize as S
import qualified Data.Set as Set
import qualified Network.Haskoin.Block as Btc

indexServer :: IndexApi AsServerM
indexServer = IndexApi
    { indexGetHeight = indexGetHeightEndpoint
    , indexGetBlockFilters = indexGetBlockFiltersEndpoint
    , indexGetInfo = indexGetInfoEndpoint
    , indexPing = indexPingEndpoint
    , indexIntroducePeer = introducePeerEndpoint
    , indexKnownPeers = knownPeersEndpoint
    , indexGetFees    = getFeesEndpoint
    }

--Endpoints
indexGetHeightEndpoint :: HeightRequest -> ServerM HeightResponse
indexGetHeightEndpoint (HeightRequest currency) = do
  mh <- getScannedHeight currency
  pure $ HeightResponse $ fromMaybe 0 mh

getBlockMetaSlice :: Currency -> BlockHeight -> BlockHeight -> ServerM [BlockMetaRec]
getBlockMetaSlice currency startHeight endHeight = do
  let start = metaRecKey (currency, startHeight)
      end   = BlockMetaRecKey currency $ startHeight + pred endHeight
  slice <- safeEntrySlice start end
  let metaSlice = snd <$> slice
  pure metaSlice

indexGetBlockFiltersEndpoint :: BlockFiltersRequest -> ServerM BlockFiltersResponse
indexGetBlockFiltersEndpoint request = do
    slice <- getBlockMetaSlice (filtersReqCurrency request) (filtersReqStartHeight request) (filtersReqAmount request)
    let blockFilters = (\s -> (blockMetaRecHeaderHashHexView s, blockMetaRecAddressFilterHexView s)) <$> slice
    pure blockFilters

indexPingEndpoint :: ServerM ()
indexPingEndpoint = void $ getParsedExact @SchemaVersionRec schemaVersionRecKey

indexGetInfoEndpoint :: ServerM InfoResponse
indexGetInfoEndpoint = do
  scanInfo <- scanningInfo
  let mappedScanInfo = scanNfoItem <$> scanInfo
  pure $ InfoResponse mappedScanInfo
  where
    scanNfoItem nfo = ScanProgressItem (nfoCurrency nfo) undefined (nfoActualHeight nfo)

introducePeerEndpoint :: IntroducePeerReq -> ServerM IntroducePeerResp
introducePeerEndpoint request = do
  result <- runExceptT $ considerPeerCandidate $ PeerCandidate $ introducePeerReqUrl $ request
  pure $ peerValidationToResponce result

peerValidationToResponce :: Either PeerValidationResult () -> IntroducePeerResp
peerValidationToResponce = \case
  Right ()   -> IntroducePeerResp True Nothing
  Left error -> IntroducePeerResp False $ Just $ case error of
    UrlFormatError ->
      "Do not able to parse address"
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

currencyToCurrencyCode :: Currency -> IPT.CurrencyCode
currencyToCurrencyCode c = case c of
  BTC -> IPT.BTC
  ERGO -> IPT.ERGO

currencyCodeToCurrency :: IPT.CurrencyCode -> Currency
currencyCodeToCurrency c = case c of
  IPT.BTC -> BTC
  IPT.ERGO -> ERGO

getFeesEndpoint :: [Currency] -> ServerM IndexFeesResp
getFeesEndpoint curs = do
  feeVar <- asks envFeeEstimates
  fees <- liftIO $ readTVarIO feeVar
  let fees' = M.mapKeys currencyCodeToCurrency fees
  pure $ IndexFeesResp $ restrictKeys fees' $ Set.fromList curs
