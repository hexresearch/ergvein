module Control.Monad.Haskey.State(
    applyState
  ) where 

import Control.Lens
import Control.Monad.Haskey
import Control.Monad.State.Strict
import Data.BTree.Alloc

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

self :: Lens' a a
self = lens id $ \_ x -> x

applyState :: MonadState s m => (s -> m s) -> m ()
applyState m = do
  b <- m =<< get
  self .= b
