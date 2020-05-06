module Ergvein.Wallet.Headers.Types(
    HeadersStorage
  , HasHeadersStorage(..)
  ) where

import Control.Monad.Reader
import Database.LMDB.Simple

type HeadersStorage = Environment ReadWrite

class Monad m => HasHeadersStorage m where
  getHeadersStorage :: m HeadersStorage

instance Monad m => HasHeadersStorage (ReaderT HeadersStorage m) where
  getHeadersStorage = ask
  {-# INLINE getHeadersStorage #-}
