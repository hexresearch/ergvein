{-# LANGUAGE GADTs #-}
module Ergvein.Filters
  ( module Ergvein.Filters.Btc
  , AddrFilter(..)
  , AddrFilterTag(..)
  , BlockHeight
  , matchAddrFilter
  )
where

import           Data.Dependent.Sum
import           Data.Functor.Identity
import           Ergvein.Filters.Btc
import           Ergvein.Types.Address
import           GHC.Generics 
import           Network.Haskoin.Block (BlockHeight)

-- | Sum type that encapsulates filters for different currencies
data AddrFilter = 
    AddrFilterBtc !BtcAddrFilter 
  | AddrFilterErgo ()
  deriving (Show, Generic)
 
-- | Helper tag that binds currency with specific filter type. It is tused in 
-- dependent pair to simplify casing by currency equality when working with specific
-- currency only.
data AddrFilterTag a where 
  AFBtc  :: AddrFilterTag (BtcAddress, BtcAddrFilter)
  AFErgo :: AddrFilterTag () -- TODO add ergo filter here

-- | Match currencies in filter and address and return `Just` if they are equal.
matchAddrFilter :: EgvAddress -> AddrFilter -> Maybe (DSum AddrFilterTag Identity)
matchAddrFilter addr f = case f of 
  AddrFilterBtc btcf -> case addr of 
    BtcAddress btca -> Just $ AFBtc :=> Identity (btca, btcf) 
    _ -> Nothing 
  AddrFilterErgo _ -> Nothing -- TODO add ergo here
  