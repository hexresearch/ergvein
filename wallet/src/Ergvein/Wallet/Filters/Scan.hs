module Ergvein.Wallet.Filters.Scan(
    filterAddress
  ) where

import Control.Monad.IO.Class
import Data.Dependent.Sum
import Data.Functor.Identity
import Ergvein.Filters
import Ergvein.Types.Address
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Platform
import Network.Haskoin.Block

filterAddress :: (MonadIO m, HasFiltersStorage m) => EgvAddress -> m [BlockHash]
filterAddress addr = foldFilters (egvAddrCurrency addr) f []
  where
    f bhash gfilter acc = case matchAddrFilter addr gfilter of
      Just (AFBtc :=> Identity (caddr, cfilter)) -> case guardSegWit caddr of
        Nothing -> acc
        Just saddr -> if applyBtcFilter btcNetwork bhash cfilter saddr then bhash : acc else acc
      Just (AFErgo :=> _) -> acc -- TODO: add ergo hree
      _ -> acc
