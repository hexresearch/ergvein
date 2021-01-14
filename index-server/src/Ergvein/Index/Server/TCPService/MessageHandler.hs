module Ergvein.Index.Server.TCPService.MessageHandler where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Conversion
import Network.Socket

import Ergvein.Index.Protocol.Types as IPT
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Metrics
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery.Discovery
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Transaction
import Ergvein.Index.Server.TCPService.Connections
import Ergvein.Index.Server.TCPService.Conversions

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

getBlockMetaSlice :: Currency -> BlockHeight -> BlockHeight -> ServerM [BlockMetaRec]
getBlockMetaSlice currency startHeight amount = do
  db <- readFiltersDb
  let start = BlockMetaRecKey currency $ startHeight
      startBinary = metaRecKey (currency, startHeight)
      end = BlockMetaRecKey currency $ startHeight + amount

  slice <- safeEntrySlice currency db startBinary start end

  pure $ snd <$> slice

handleMsg :: SockAddr -> Message -> ServerM [Message]
handleMsg _ (MPing msg) = pure [MPong msg]

handleMsg _ (MPong _) = pure mempty

handleMsg _ (MVersionACK _) = pure mempty

handleMsg address (MVersion peerVersion) = do
  ownVer <- ownVersion
  liftIO $ print $ show $ protocolVersion == versionVersion peerVersion
  if protocolVersion == versionVersion peerVersion then do
    considerPeer ownVer $ PeerCandidate address $ versionScanBlocks ownVer
    liftIO $ print $ show ownVer
    pure [ MVersionACK $ VersionACK, MVersion ownVer ]
  else
    pure mempty

handleMsg _ (MPeerRequest _) = do
  knownPeers <- getActualPeers
  pure $ pure $ MPeerResponse $ PeerResponse $ V.fromList knownPeers

handleMsg _ (MFiltersRequest FilterRequest {..}) = do
  currency <- currencyCodeToCurrency filterRequestMsgCurrency
  slice <- getBlockMetaSlice currency filterRequestMsgStart filterRequestMsgAmount
  let filters = V.fromList $ convert <$> slice
  void $ addCounter filtersServedCounter $ fromIntegral $ V.length filters

  pure $ pure $ MFiltersResponse $ FilterResponse
    { filterResponseCurrency = filterRequestMsgCurrency
    , filterResponseFilters = filters
    }

handleMsg address (MFiltersEvent FilterEvent {..}) = do
  currency <- currencyCodeToCurrency filterEventCurrency
  slice <- getBlockMetaSlice currency filterEventHeight 1
  let filters = blockFilterFilter . convert <$> slice
  when (any (/= filterEventBlockFilter) filters) $ do
   closeConnection address
   deletePeerBySockAddr $ convert address
  pure mempty


handleMsg _ (MFeeRequest curs) = do
  fees <- liftIO . readTVarIO =<< asks envFeeEstimates
  let selCurs = M.restrictKeys fees $ S.fromList curs
  let resps =  (`M.mapWithKey` selCurs) $ \cur fb -> case cur of
        IPT.BTC -> FeeRespBTC False fb
        IPT.TBTC -> FeeRespBTC True fb
        _ -> let FeeBundle (_, h) (_, m) (_, l) = fb
          in FeeRespGeneric cur h m l
  pure $ pure $ MFeeResponse $ M.elems resps

handleMsg _ _ = pure []
