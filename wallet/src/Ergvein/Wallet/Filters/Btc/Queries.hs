module Ergvein.Wallet.Filters.Btc.Queries(
    insertFilter
  , getFilter
  , getFiltersHeight
  , foldFilters
  ) where

import Control.Monad.IO.Class 
import Data.Maybe
import Database.LMDB.Simple
import Ergvein.Filters.Btc 
import Ergvein.Wallet.Filters.Btc.Types
import Network.Haskoin.Block

insertFilter :: MonadIO m => BlockHeight -> BlockHash -> BtcAddrFilter -> Environment ReadWrite -> m ()
insertFilter h bh f = liftIO . readWriteTransaction $ do 
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  tdb <- getBtcTotalDb
  put fdb bh $ Just f 
  put hdb h $ Just bh 
  mtotal <- get tdb ()
  case mtotal of 
    Just total | total >= h -> put tdb () $ Just h  
    _ -> pure ()
   
getFilter :: AllocReaderM m => BlockHeight -> SchemaBtc -> m (Maybe BtcAddrFilter)
getFilter k s = do 
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  mh <- get hdb k
  case mh of 
    Nothing -> pure Nothing 
    Just h -> get fdb h
  
getFiltersHeight :: AllocReaderM m => SchemaBtc -> m BlockHeight
getFiltersHeight s = fromMaybe 0 . fmap fst <$> B.lookupMax (s ^. schemaBtcHeights)

-- | Right fold over all filters
foldFilters :: AllocReaderM m => (BlockHash -> BtcAddrFilter -> a -> a) -> a -> SchemaBtc -> m a 
foldFilters f a0 = B.foldrWithKey f a0 . view schemaBtcFilters