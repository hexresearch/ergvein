-----------------------------------------------------------------------------
--
-- Module      :  Ergvein.Wallet.Filters.Loader
-- Copyright   :  2019 ATUM SOLUTIONS AG
-- License     :  MIT
--
-- Maintainer  :  Anton Gushcha <ncrashed@protonmail.com>, Vladimir Krutkin <krutkinvs@gmail.com>
-- Stability   :
-- Portability :
--
-- | Contains thread that continuously loads golomb rice filters from indexers.
--
-----------------------------------------------------------------------------

module Ergvein.Wallet.Filters.Loader (
  getFilters
) where

import Data.ByteString (ByteString)

import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Util

import qualified Data.Vector as V

getFilters :: MonadFront t m => Currency -> Event t (BlockHeight, Int) -> m (Event t [(BlockHash, ByteString)])
getFilters cur e = do
  respE <- requestRandomIndexer $ ffor e $ \(h, n) -> (cur, ) $
    MFiltersRequest $ FilterRequest curcode (fromIntegral h) (fromIntegral n)
  let respE' = fforMaybe respE $ \case
        (addr, MFiltersResponse (FilterResponse{..})) -> if filterResponseCurrency /= curcode
          then Nothing
          else Just $ (addr,) $ V.toList $ ffor filterResponseFilters $ \(BlockFilter bid filt) -> (bid, filt)
        _ -> Nothing
  warnDesynced respE'
  pure $ snd <$> respE'
  where
    curcode = currencyToCurrencyCode cur

warnDesynced :: MonadFront t m => Event t (SockAddr, [a]) -> m ()
warnDesynced e = showWarnMsg $ fforMaybe e $ \(addr, rs) -> if null rs
  then Just $ "Indexer " <> showt addr <> " possibly out of sync!"
  else Nothing
