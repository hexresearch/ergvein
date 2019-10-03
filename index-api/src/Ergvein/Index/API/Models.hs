module Ergvein.Index.API.Models where

import Data.Word
import Data.Text
import Ergvein.Aeson

type LockingScriptHash = Text

data Currency = BTC | ERGO

data BalanceResponce = BalanceResponce 
    { confirmed   :: !Word64
    , unconfirmed :: !Word64
    }
$(deriveJSON (aesonOptionsStripPrefix "balanceResponce") ''BalanceResponce)