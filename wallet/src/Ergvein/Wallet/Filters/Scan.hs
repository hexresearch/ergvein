module Ergvein.Wallet.Filters.Scan(
    filterAddress
  , filterBtcAddresses
  ) where

import Control.Monad.IO.Class
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Vector (Vector)
import Ergvein.Filters.Mutable
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Platform
import Network.Haskoin.Block

import qualified Data.Vector as V

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

-- | Scan through unprocessed filters and return scanned height and matches. The function
-- expect that all addresses are for the same currency.
filterBtcAddresses :: (MonadIO m, HasFiltersStorage t m) => Vector BtcAddress -> m (BlockHeight, Vector BlockHash)
filterBtcAddresses as
  | V.null as = pure (0, mempty)
  | otherwise = scanBtcFilters f (filterStartingHeight BTC, mempty)
  where
    f i bhash cfilter (!_, !acc) = do
      res <- checkBtcAddresses bhash cfilter as
      let acc' = if res then V.cons bhash acc else acc
      pure (i, acc')

checkBtcAddresses :: MonadIO m => BlockHash -> BtcAddrFilter -> Vector BtcAddress -> m Bool
checkBtcAddresses bhash cfilter cas
  | V.null cas = pure False
  | otherwise = let
    caddr = V.head cas
    in case guardSegWit caddr of
          Nothing -> pure False
          Just saddr -> do
            res <- applyBtcFilter btcNetwork bhash cfilter saddr
            if res then pure True else checkBtcAddresses bhash cfilter (V.drop 1 cas)
