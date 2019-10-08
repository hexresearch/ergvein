module Ergvein.Index.API where

import Servant.API
import Servant.API.Generic
import Ergvein.Index.API.V1
import Ergvein.Types.Currency

data IndexVersionedApi route = IndexVersionedApi
    { 
        indexVersionedApi'v1 :: route :- "api" :> "v1" :> QueryParam "currency" Currency :> ToServant IndexApi AsApi
    } deriving Generic