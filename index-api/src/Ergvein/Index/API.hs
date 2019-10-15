module Ergvein.Index.API where

import Data.Proxy
import Servant.API
import Servant.API.Generic
import Ergvein.Index.API.V1
import Ergvein.Types.Currency

data IndexVersionedApi route = IndexVersionedApi
    { indexVersionedApi'v1 :: route :- "api" :> "v1" :> ToServant IndexApi AsApi
    } deriving (Generic)

indexApi :: Proxy (ToServantApi IndexVersionedApi)
indexApi = genericApi (Proxy ::  Proxy IndexVersionedApi)