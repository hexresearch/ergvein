module Ergvein.Index.Server.Server.V1 where

import Data.Proxy
import Servant.Server
import Servant.API.Generic
import Servant.Server.Generic

import Ergvein.Types.Currency
import Ergvein.Index.API.Types
import Ergvein.Index.API
import Ergvein.Index.API.V1

indexServer :: IndexApi AsServer
indexServer = IndexApi 
    { indexGetBalance = indexGetBalanceEndpoint
    }  

--indexGetBalanceEndpoint :: PubKeyScriptHash -> Server BalanceResponse
indexGetBalanceEndpoint x = do
    pure BalanceResponse { balRespConfirmed = 0 , balRespUnconfirmed = 0 }

--ergveinIndexServer :: IndexVersionedApi AsServer
ergveinIndexServer = IndexVersionedApi {
    indexVersionedApi'v1  = ToServant indexServer
    }

indexApi :: Proxy (ToServantApi IndexVersionedApi)
indexApi = genericApi (Proxy ::  Proxy IndexVersionedApi)
