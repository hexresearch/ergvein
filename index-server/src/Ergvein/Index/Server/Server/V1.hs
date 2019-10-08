module Ergvein.Index.Server.Server.V1 where

import Data.Proxy
import Servant.Server
import Servant.API.Generic
import Servant.Server.Generic

import Ergvein.Types.Currency
import Ergvein.Index.API.Types
import Ergvein.Index.API
import Ergvein.Index.API.V1

indexServer :: Maybe Currency -> IndexApi AsServer
indexServer _ = IndexApi 
    { indexGetBalance = indexGetBalanceEndpoint
    , indexGetTxHashHistory = undefined
    , indexGetTxMerkleProof = undefined
    , indexGetTxHexView = undefined
    , indexGetTxFeeHistogram = undefined
    , indexTxBroadcast = undefined
    }  

indexGetBalanceEndpoint :: Maybe PubKeyScriptHash -> Handler BalanceResponse
indexGetBalanceEndpoint x = do
    pure BalanceResponse { balRespConfirmed = 0 , balRespUnconfirmed = 0 }

ergveinIndexServer :: IndexVersionedApi AsServer
ergveinIndexServer = IndexVersionedApi {
    indexVersionedApi'v1  = \mcur -> toServant $ indexServer mcur
    }

indexApi :: Proxy (ToServantApi IndexVersionedApi)
indexApi = genericApi (Proxy ::  Proxy IndexVersionedApi)
