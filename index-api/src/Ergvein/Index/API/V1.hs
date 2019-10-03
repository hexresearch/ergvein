module Ergvein.Index.API.V1 where

import Servant.API
import Servant.API.Generic
import Ergvein.Types.Currency
import Ergvein.Index.API.Types

type IndexGetBalance = QueryParam "lockingScriptHash" PubKeyScriptHash :> Get '[JSON] BalanceResponse

data IndexApi route = IndexApi
    { indexApiBalance :: route :- IndexGetBalance
    }