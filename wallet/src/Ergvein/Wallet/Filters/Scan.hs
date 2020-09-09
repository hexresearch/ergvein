module Ergvein.Wallet.Filters.Scan(
    filterAddress
  , filterBtcAddress
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
import Ergvein.Text

import qualified Data.Vector as V

filterAddress :: (MonadIO m, HasFiltersStorage t m) => EgvAddress -> m [BlockHash]
filterAddress addr = foldFilters (egvAddrCurrency addr) f []
  where
    f _ _ bhash gfilter acc = case matchAddrFilter addr gfilter of
      Just (AFBtc :=> Identity (caddr, cfilter)) -> do
          res <- applyBtcFilter bhash cfilter $ addressToScriptBS caddr
          pure $ if res then bhash : acc else acc
      Just (AFErgo :=> _) -> pure acc -- TODO: add ergo hree
      _ -> pure acc

-- | Scan through unprocessed filters and return scanned height and matches.
filterBtcAddress :: (MonadIO m, HasFiltersStorage t m)
  => BlockHeight                              -- ^ Starting height
  -> (BlockHeight -> BlockHeight -> IO ())    -- ^ Progress logging callback
  -> BtcAddress                               -- ^ Address to match
  -> m (BlockHeight, Vector (BlockHash, BlockHeight))        -- ^ Scanned height and matches
filterBtcAddress i0 progCb ba = scanBtcFilters i0 f (filterStartingHeight BTC, mempty)
  where
    f i n bhash cfilter (!_, !acc) = do
      liftIO $ progCb i n
      res <- applyBtcFilter bhash cfilter $ addressToScriptBS ba
      let acc' = if res then V.cons (bhash, i) acc else acc
      pure (i, acc')

-- | Scan through unprocessed filters and return scanned height and matches. The function
-- expect that all addresses are for the same currency.
filterBtcAddresses :: (MonadIO m, HasFiltersStorage t m)
  => BlockHeight                              -- ^ Starting height
  -> (BlockHeight -> BlockHeight -> IO ())    -- ^ Progress logging callback
  -> Vector BtcAddress                        -- ^ Addresses to match
  -> m (BlockHeight, Vector (BlockHash, BlockHeight))        -- ^ Scanned height and matches
filterBtcAddresses i0 progCb as
  | V.null bas = pure (0, mempty)
  | otherwise = scanBtcFilters i0 f (filterStartingHeight BTC, mempty)
  where
    bas = V.map addressToScriptBS as
    f i n bhash cfilter (!_, !acc) = do
      liftIO $ progCb i n
      res <- applyBtcFilterMany bhash cfilter $ V.toList bas
      let acc' = if res then V.cons (bhash, i) acc else acc
      pure (i, acc')
