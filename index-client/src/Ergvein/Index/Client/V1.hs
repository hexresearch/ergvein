module Ergvein.Index.Client.V1
  (
    HasClientManager(..)
  , getHeightEndpoint
  , getBlockFiltersEndpoint
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

getBlockFiltersEndpoint :: HasClientManager m => BaseUrl -> BlockFiltersRequest -> m (Either ClientError BlockFiltersResponse)
getBlockFiltersEndpoint url req = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetBlockFilters apiV1 req

getInfoEndpoint :: HasClientManager m => BaseUrl -> () -> m (Either ClientError InfoResponse)
getInfoEndpoint url _ = do
  cenv <- fmap (`mkClientEnv` url) getClientMaganer
  liftIO $ flip runClientM cenv $ indexGetInfo apiV1
