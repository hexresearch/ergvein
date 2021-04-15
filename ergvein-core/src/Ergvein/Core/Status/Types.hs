module Ergvein.Core.Status.Types(
    SyncBehind(..)
  , StatusUpdate(..)
  , CurrencyStatus(..)
  , nominalToBehind
  ) where

import Data.Time
import Ergvein.Text
import Ergvein.Types.Currency

data SyncBehind = SyncDays !Int | SyncHours !Int

nominalToBehind :: NominalDiffTime -> SyncBehind
nominalToBehind t
  | t < 24 * 3600 = SyncHours $ ceiling $ t / 3600
  | otherwise = SyncDays $ ceiling $ t / (24 * 3600)

data StatusUpdate
  = StatGettingNodeAddresses
  | StatConnectingToPeers
  | StatGettingHeight !Int             -- Current height for catch up
  | StatNewFilters !Int
  | Synced
  | NotActive
  deriving (Show, Eq, Ord)

data CurrencyStatus = CurrencyStatus !Currency !StatusUpdate
  deriving (Show, Eq)
