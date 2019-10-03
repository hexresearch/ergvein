module Ergvein.Index.API.Types where

import Ergvein.Aeson
import Ergvein.Types.Currency

data BalanceResponse = BalanceResponse 
    { balRespConfirmed   :: !MoneyUnit
    , balRespUnconfirmed :: !MoneyUnit
    }
$(deriveJSON (aesonOptionsStripPrefix "balResp") ''BalanceResponse)