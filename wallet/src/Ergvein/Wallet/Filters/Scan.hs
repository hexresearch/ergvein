module Ergvein.Wallet.Filters.Scan(
    filterAddress
  ) where 

import Ergvein.Filters
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Filters.Types

filterAddress :: HasFiltersStorage m => EgvAddress -> m [BlockHeight]
filterAddress addr = pure []