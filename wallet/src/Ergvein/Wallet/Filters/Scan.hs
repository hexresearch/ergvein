module Ergvein.Wallet.Filters.Scan(
    filterAddress
  , filterAddresses
  ) where

import Control.Monad.IO.Class
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Vector (Vector)
import Ergvein.Filters.Mutable
import Ergvein.Types.Address
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Platform
import Network.Haskoin.Block

filterAddress :: (MonadIO m, HasFiltersStorage t m) => EgvAddress -> m [BlockHash]
filterAddress addr = foldFilters (egvAddrCurrency addr) f []
  where
    f bhash gfilter acc = case matchAddrFilter addr gfilter of
      Just (AFBtc :=> Identity (caddr, cfilter)) -> case guardSegWit caddr of
        Nothing -> pure acc
        Just saddr -> do
          res <- applyBtcFilter btcNetwork bhash cfilter saddr
          pure $ if res then bhash : acc else acc
      Just (AFErgo :=> _) -> pure acc -- TODO: add ergo hree
      _ -> pure acc

-- | Scan through unprocessed filters and return scanned height and matches.
filterAddresses :: (MonadIO m, HasFiltersStorage t m) => Vector EgvAddress -> m (BlockHeight, Vector BlockHash)
filterAddresses _ = pure (0, mempty)
