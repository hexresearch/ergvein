module Ergvein.Core.Filters(
  getFilters
) where

import Data.ByteString (ByteString)

import Ergvein.Core.Wallet
import Ergvein.Core.Currency
import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Vector as V

getFilters :: MonadWallet t m => Currency -> Event t (BlockHeight, Int) -> m (Event t [(BlockHash, ByteString)])
getFilters cur e = do
  respE <- requestRandomIndexer $ ffor e $ \(h, n) -> (cur, ) $
    MFiltersRequest $ FilterRequest curcode (fromIntegral h) (fromIntegral n)
  let respE' = fforMaybe respE $ \case
        (addr, MFiltersResponse (FilterResponse{..})) -> if filterResponseCurrency /= curcode
          then Nothing
          else Just $ (addr,) $ V.toList $ ffor filterResponseFilters $ \(BlockFilter bid filt) -> (bid, filt)
        _ -> Nothing
  pure $ snd <$> respE'
  where
    curcode = currencyToCurrencyCode cur
