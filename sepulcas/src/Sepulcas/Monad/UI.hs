module Sepulcas.Monad.UI where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Reader

class MonadIO m => MonadHasUI m where
  -- | Internal method of getting channel where you can post actions that must be
  -- executed in main UI thread.
  getUiChan :: m (Chan (IO ()))

instance MonadIO m => MonadHasUI (ReaderT (Chan (IO ())) m) where
  getUiChan = ask
  {-# INLINE getUiChan #-}

instance MonadIO m => MonadHasUI (ReaderT (Chan (IO ()), a) m) where
  getUiChan = asks fst
  {-# INLINE getUiChan #-}
