module Ergvein.Index.API where

import Servant.API
import Servant.API.Generic
import Ergvein.Index.API.V1
import Ergvein.Index.API.Models

data IndexVersionedApi route = IndexVersionedApi
    { 
        indexVersionedApi'v1 :: route :- "api" :> "v1" :> QueryParam "currency" Currency :> ToServant IndexApi AsApi
    }