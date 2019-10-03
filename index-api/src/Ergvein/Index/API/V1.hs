module Ergvein.Index.API.V1 where

import Servant.API
import Servant.API.Generic
import Ergvein.Index.API.Models

type IndexGetBalance = QueryParam "lockingScriptHash" LockingScriptHash :> Get '[JSON] BalanceResponce

data IndexApi route = IndexApi
    { indexApiBalance :: route :- IndexGetBalance
    }