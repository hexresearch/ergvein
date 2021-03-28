module Ergvein.Index.Server.TCPService.MessageHandler
  ( handleMsg
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Short (toShort, fromShort)
import Data.List (foldl')
import Network.Socket

import Ergvein.Index.Protocol.Types as IPT
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Queries.Peers
import Ergvein.Index.Server.Metrics
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.PeerDiscovery
import Ergvein.Index.Server.TCPService.Conversions
import Ergvein.Index.Server.Types
import Ergvein.Text
import Ergvein.Types.Fees

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

handleMsg :: ServerMonad m
  => SockAddr -> Message -> m ([Message], Bool) -- bool is to close connection
handleMsg _ (MPing msg) = pure ([MPong msg], False)

handleMsg _ (MPong _) = pure (mempty, False)

handleMsg _ (MVersionACK _) = pure (mempty, False)

handleMsg addr (MReject r) = do
  logErrorN $ "<" <> showt addr <> ">: Client sent reject: " <> showt r
  pure (mempty, True)

handleMsg address (MVersion peerVersion) =
  if protocolVersion `isCompatible` versionVersion peerVersion then do
    ownVer <- ownVersion
    considerPeer ownVer $ PeerCandidate address $ versionScanBlocks ownVer
    pure ([ MVersionACK VersionACK, MVersion ownVer ], False)
  else
    pure ([ MReject $ Reject MVersionType VersionNotSupported $ "Given version is not compatible with " <> showProtocolVersion protocolVersion], True)

handleMsg _ (MPeerRequest _) = do
  knownPeers <- getActualPeers
  pure ([MPeerResponse $ PeerResponse $ V.fromList knownPeers], False)

handleMsg _ (MFiltersRequest FilterRequest {..}) = do
  currency <- currencyCodeToCurrency filterRequestMsgCurrency
  slice <- getFiltersSlice currency (\s f -> BlockFilter (toShort s) f) filterRequestMsgStart filterRequestMsgAmount
  let filters = V.fromList slice
  let ff = flip fmap slice $ \(BlockFilter s f) -> (bs2Hex $ fromShort s, bs2Hex f)
  void $ addCounter filtersServedCounter $ fromIntegral $ V.length filters

  liftIO $ print filterRequestMsgCurrency
  liftIO $ print ff

  pure ([MFiltersResponse $ FilterResponse
    { filterResponseCurrency = filterRequestMsgCurrency
    , filterResponseFilters = filters
    }], False)

handleMsg address (MFiltersEvent FilterEvent {..}) = do
  currency <- currencyCodeToCurrency filterEventCurrency
  mfilt <- getSingleFilter currency filterEventHeight
  if any ((/=) filterEventBlockFilter . snd) mfilt
    then deletePeerBySockAddr address >> pure ([], True)
    else pure ([], False)

handleMsg _ (MFeeRequest curs) = do
  fees <- getFees
  let selCurs = M.restrictKeys fees $ S.fromList curs
  let resps =  (`M.mapWithKey` selCurs) $ \cur fb -> case cur of
        IPT.BTC -> FeeRespBTC False fb
        IPT.TBTC -> FeeRespBTC True fb
        _ -> let FeeBundle (_, h) (_, m) (_, l) = fb
          in FeeRespGeneric cur h m l
  pure $ ([MFeeResponse $ M.elems resps], False)

handleMsg _ (MRatesRequest (RatesRequest rs)) = do
  rates <- liftIO . readTVarIO =<< getRatesVar
  let boo fs m = let m' = M.restrictKeys m $ S.fromList fs
        in if M.null m' then Nothing else Just m'
  let foo m (c,fs) = M.update (boo fs) c m
  let resp = MRatesResponse $ RatesResponse $ foldl' foo rates $ M.toList rs
  pure ([resp], False)

handleMsg _ _ = pure ([], False)
