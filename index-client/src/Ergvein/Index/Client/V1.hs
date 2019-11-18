module Ergvein.Index.Client.V1
  (
    HasClientManager(..)
  , getBalanceEndpoint
  , getTxHashHistoryEndpoint
  , getTxMerkleProofEndpoint
  , getTxHexViewEndpoint
  , getTxFeeHistogramEndpoint
  , txBroadcastEndpoint
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

getBalanceEndpoint :: HasClientManager m => BaseUrl -> BalanceRequest -> m (Either ServantError BalanceResponse)
getBalanceEndpoint url req = do
  cenv <- fmap (flip mkClientEnv url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetBalance apiV1 req

getTxHashHistoryEndpoint :: HasClientManager m => BaseUrl -> TxHashHistoryRequest-> m (Either ServantError TxHashHistoryResponse)
getTxHashHistoryEndpoint url req = do
  cenv <- fmap (flip mkClientEnv url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetTxHashHistory apiV1 req

getTxMerkleProofEndpoint :: HasClientManager m => BaseUrl -> TxMerkleProofRequest -> m (Either ServantError TxMerkleProofResponse)
getTxMerkleProofEndpoint url req = do
  cenv <- fmap (flip mkClientEnv url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetTxMerkleProof apiV1 req

getTxHexViewEndpoint :: HasClientManager m => BaseUrl -> TxHexViewRequest -> m (Either ServantError TxHexViewResponse)
getTxHexViewEndpoint url req = do
  cenv <- fmap (flip mkClientEnv url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetTxHexView apiV1 req

getTxFeeHistogramEndpoint :: HasClientManager m => BaseUrl -> TxFeeHistogramRequest -> m (Either ServantError TxFeeHistogramResponse)
getTxFeeHistogramEndpoint url req = do
  cenv <- fmap (flip mkClientEnv url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetTxFeeHistogram apiV1 req

txBroadcastEndpoint :: HasClientManager m => BaseUrl -> TxBroadcastRequest -> m (Either ServantError TxBroadcastResponse)
txBroadcastEndpoint url req = do
  cenv <- fmap (flip mkClientEnv url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexTxBroadcast apiV1 req
