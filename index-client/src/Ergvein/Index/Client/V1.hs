module Ergvein.Index.Client.V1
  (
    HasClientManager(..)
  , getHeightEndpoint
  , getBalanceEndpoint
  , getBlockFiltersEndpoint
  , getTxHashHistoryEndpoint
  , getTxMerkleProofEndpoint
  , getTxHexViewEndpoint
  , getTxFeeHistogramEndpoint
  , txBroadcastEndpoint
  , getInfoEndpoint
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Proxy
import Servant.API
import Servant.API.Generic
import Servant.Client

import Ergvein.Index.API
import Ergvein.Index.API.Types
import Ergvein.Index.API.V1
import Ergvein.Text
import Ergvein.Wallet.Native
import Network.HTTP.Client hiding (Proxy)

data AsClient

instance GenericMode AsClient where
  type AsClient :- api = Client ClientM api

api :: IndexVersionedApi AsClient
api = fromServant $ client (Proxy :: Proxy (ToServantApi IndexVersionedApi))

apiV1 :: IndexApi AsClient
apiV1 = fromServant $ indexVersionedApi'v1 api

class MonadIO m => HasClientManager m where
  getClientMaganer  :: m Manager

getHeightEndpoint :: (HasClientManager m, PlatformNatives) => BaseUrl -> HeightRequest -> m (Either ClientError HeightResponse)
getHeightEndpoint url req = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  res <- liftIO $ flip runClientM cenv $ indexGetHeight apiV1 req
  liftIO $ case res of
    Left er -> logWrite $ showt er
    _ -> pure ()
  pure res

getBalanceEndpoint :: HasClientManager m => BaseUrl -> BalanceRequest -> m (Either ClientError BalanceResponse)
getBalanceEndpoint url req = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetBalance apiV1 req

getBlockFiltersEndpoint :: HasClientManager m => BaseUrl -> BlockFiltersRequest -> m (Either ClientError BlockFiltersResponse)
getBlockFiltersEndpoint url req = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetBlockFilters apiV1 req

getTxHashHistoryEndpoint :: HasClientManager m => BaseUrl -> TxHashHistoryRequest-> m (Either ClientError TxHashHistoryResponse)
getTxHashHistoryEndpoint url req = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetTxHashHistory apiV1 req

getTxMerkleProofEndpoint :: HasClientManager m => BaseUrl -> TxMerkleProofRequest -> m (Either ClientError TxMerkleProofResponse)
getTxMerkleProofEndpoint url req = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetTxMerkleProof apiV1 req

getTxHexViewEndpoint :: HasClientManager m => BaseUrl -> TxHexViewRequest -> m (Either ClientError TxHexViewResponse)
getTxHexViewEndpoint url req = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetTxHexView apiV1 req

getTxFeeHistogramEndpoint :: HasClientManager m => BaseUrl -> TxFeeHistogramRequest -> m (Either ClientError TxFeeHistogramResponse)
getTxFeeHistogramEndpoint url req = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetTxFeeHistogram apiV1 req

txBroadcastEndpoint :: HasClientManager m => BaseUrl -> TxBroadcastRequest -> m (Either ClientError TxBroadcastResponse)
txBroadcastEndpoint url req = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexTxBroadcast apiV1 req

getInfoEndpoint :: HasClientManager m => BaseUrl -> () -> m (Either ClientError InfoResponse)
getInfoEndpoint url _ = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetInfo apiV1
