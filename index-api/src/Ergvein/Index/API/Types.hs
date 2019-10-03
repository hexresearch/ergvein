module Ergvein.Index.API.Types where

import Ergvein.Aeson
import Ergvein.Types.Currency

data BalanceResponse = BalanceResponce 
    { balRespСonfirmed   :: !MoneyUnit
    , balRespUnconfirmed :: !MoneyUnit
    }
$(deriveJSON (aesonOptionsStripPrefix "balResp") ''BalanceResponse)