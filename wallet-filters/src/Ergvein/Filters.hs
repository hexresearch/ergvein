module Ergvein.Filters
  ( module Ergvein.Filters.Btc
  , AddrFilter(..)
  , BlockHeight
  )
where

import           Ergvein.Filters.Btc
import           GHC.Generics 
import           Network.Haskoin.Block (BlockHeight)

-- | Sum type that encapsulates filters for different currencies
data AddrFilter = AddrFilterBtc !BtcAddrFilter
  deriving (Show, Generic)