module Ergvein.Wallet.Headers.Btc.Queries(
    insertHeader
  , getHeader
  , setBestHeader
  , getBestHeader
  ) where

import Control.Lens
import Control.Monad.Haskey
import Control.Monad.State.Strict
import Data.BTree.Alloc
import Data.Maybe
import Ergvein.Wallet.Headers.Btc.Types
import Network.Haskoin.Block

import qualified Data.BTree.Impure as B

insertHeader :: AllocM m => BlockNode -> SchemaBtc -> m SchemaBtc
insertHeader n = schemaBtcHeaders %%~ B.insert (blockNodeId n) n

getHeader :: AllocReaderM m => BlockHash -> SchemaBtc -> m (Maybe BlockNode)
getHeader k s = B.lookup k (s ^. schemaBtcHeaders)

setBestHeader :: AllocM m => BlockNode -> SchemaBtc -> m SchemaBtc
setBestHeader n = schemaBtcBestBlock %%~ B.insert () n

getBestHeader :: AllocReaderM m => SchemaBtc -> m BlockNode
getBestHeader s = fromMaybe defaultBestBlock <$> B.lookup () (s ^. schemaBtcBestBlock)

instance AllocReaderM m => AllocReaderM (StateT s m) where
  readNode k v = lift $ readNode k v
  {-# INLINE readNode #-}
  readOverflow = lift . readOverflow
  {-# INLINE readOverflow #-}

instance AllocM m => AllocM (StateT s m) where
  nodePageSize = lift nodePageSize
  {-# INLINE nodePageSize #-}
  maxPageSize = lift maxPageSize
  {-# INLINE maxPageSize #-}
  maxKeySize = lift maxKeySize
  {-# INLINE maxKeySize #-}
  maxValueSize = lift maxValueSize
  {-# INLINE maxValueSize #-}
  allocNode h n = lift $ allocNode h n
  {-# INLINE allocNode #-}
  freeNode h n = lift $ freeNode h n
  {-# INLINE freeNode #-}
  allocOverflow = lift . allocOverflow
  {-# INLINE allocOverflow #-}
  freeOverflow = lift . freeOverflow
  {-# INLINE freeOverflow #-}
  deleteOverflowData = lift . deleteOverflowData
  {-# INLINE deleteOverflowData #-}

instance AllocM m => BlockHeaders (StateT SchemaBtc m) where
  addBlockHeader = applyState . insertHeader
  getBlockHeader h = getHeader h =<< get
  getBestBlockHeader = getBestHeader =<< get
  setBestBlockHeader = applyState . setBestHeader
  {-# INLINE addBlockHeader #-}
  {-# INLINE getBlockHeader #-}
  {-# INLINE getBestBlockHeader #-}
  {-# INLINE setBestBlockHeader #-}

self :: Lens' a a
self = lens id $ \_ x -> x

applyState :: MonadState s m => (s -> m s) -> m ()
applyState m = do
  b <- m =<< get
  self .= b
