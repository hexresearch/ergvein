module Ergvein.Wallet.Filters.Btc.Queries(
    insertFilter
  , getFilter
  , getFiltersHeight
  , foldFilters
  ) where

import Control.Lens
import Control.Monad.Haskey
import Control.Monad.State.Strict
import Data.BTree.Alloc
import Data.Maybe
import Ergvein.Filters.Btc 
import Ergvein.Wallet.Filters.Btc.Types
import Network.Haskoin.Block

import qualified Data.BTree.Impure as B

insertFilter :: AllocM m => BlockHeight -> BlockHash -> BtcAddrFilter -> SchemaBtc -> m SchemaBtc
insertFilter h bh f = schemaBtcFilters %%~ B.insert bh f
                  >=> schemaBtcHeights %%~ B.insert h bh

getFilter :: AllocReaderM m => BlockHeight -> SchemaBtc -> m (Maybe BtcAddrFilter)
getFilter k s = do 
  mh <- B.lookup k (s ^. schemaBtcHeights)
  case mh of 
    Nothing -> pure Nothing 
    Just h -> B.lookup h (s ^. schemaBtcFilters)

getFiltersHeight :: AllocReaderM m => SchemaBtc -> m BlockHeight
getFiltersHeight s = fromMaybe 0 . fmap fst <$> B.lookupMax (s ^. schemaBtcHeights)

-- | Right fold over all filters
foldFilters :: AllocReaderM m => (BlockHash -> BtcAddrFilter -> a -> a) -> a -> SchemaBtc -> m a 
foldFilters f a0 = B.foldrWithKey f a0 . view schemaBtcFilters