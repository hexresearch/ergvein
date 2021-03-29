module Ergvein.Index.Server.TCPService.MessageHandler
  ( handleMsg
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Conversion
import Data.List (foldl')
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
import Ergvein.Index.Server.TCPService.Conversions
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Transaction

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Vector     as V

getBlockMetaSlice :: Currency -> BlockHeight -> BlockHeight -> ServerM [BlockInfoRec]
getBlockMetaSlice currency startHeight amount = do
  db <- getFiltersDb
  let start = BlockInfoRecKey currency $ startHeight
      startBinary = blockInfoRecKey (currency, startHeight)
      end = BlockInfoRecKey currency $ startHeight + amount

  slice <- safeEntrySlice currency db startBinary start end

  pure $ snd <$> slice

handleMsg :: SockAddr -> Message -> ServerM (Maybe Message, Bool) -- bool is to close connection
handleMsg _ (MPing msg) = pure (Just $ MPong msg, False)

handleMsg _ (MPong _) = pure (Nothing, False)

handleMsg _ (MVersionACK _) = pure (Nothing, False)

handleMsg addr (MReject r) = do
  logErrorN $ "<" <> showt addr <> ">: Client sent reject: " <> showt r
  pure (Nothing, True)

handleMsg address (MVersion peerVersion) =
  if protocolVersion `isCompatible` versionVersion peerVersion then do
    ownVer <- ownVersion
    considerPeer ownVer $ PeerCandidate address $ versionScanBlocks ownVer
    pure (Just $ MVersionACK VersionACK, False)
  else
    pure (Just $ MReject $ Reject MVersionType VersionNotSupported $ "Given version is not compatible with " <> showProtocolVersion protocolVersion, True)

handleMsg _ (MPeerRequest _) = do
  knownPeers <- getActualPeers
  pure (Just $ MPeerResponse $ PeerResponse $ V.fromList knownPeers, False)

handleMsg _ (MFiltersRequest FilterRequest {..}) = do
  currency <- currencyCodeToCurrency filterRequestMsgCurrency
  slice <- getBlockMetaSlice currency filterRequestMsgStart filterRequestMsgAmount
  let filters = V.fromList $ convert <$> slice
  void $ addCounter filtersServedCounter $ fromIntegral $ V.length filters

  pure (Just $ MFiltersResponse $ FilterResponse
    { filterResponseCurrency = filterRequestMsgCurrency
    , filterResponseFilters = filters
    }, False)

handleMsg address (MFiltersEvent FilterEvent {..}) = do
  currency <- currencyCodeToCurrency filterEventCurrency
  slice <- getBlockMetaSlice currency filterEventHeight 1
  let filters = blockFilterFilter . convert <$> slice
  case any (/= filterEventBlockFilter) filters of
    True  -> do deletePeerBySockAddr $ convert address
                pure (Nothing, True)
    False -> do pure (Nothing, False)

handleMsg _ (MFeeRequest curs) = do
  fees <- liftIO . readTVarIO =<< asks envFeeEstimates
  let selCurs = M.restrictKeys fees $ S.fromList curs
  let resps =  (`M.mapWithKey` selCurs) $ \cur fb -> case cur of
        IPT.BTC -> FeeRespBTC False fb
        IPT.TBTC -> FeeRespBTC True fb
        _ -> let FeeBundle (_, h) (_, m) (_, l) = fb
          in FeeRespGeneric cur h m l
  pure $ (Just $ MFeeResponse $ M.elems resps, False)

handleMsg _ (MRatesRequest (RatesRequest rs)) = do
  rates <- liftIO . readTVarIO =<< asks envExchangeRates
  let boo fs m = let m' = M.restrictKeys m $ S.fromList fs
        in if M.null m' then Nothing else Just m'
  let foo m (c,fs) = M.update (boo fs) c m
  let resp = MRatesResponse $ RatesResponse $ foldl' foo rates $ M.toList rs
  pure (Just resp, False)

handleMsg _ _ = pure (Nothing, False)
