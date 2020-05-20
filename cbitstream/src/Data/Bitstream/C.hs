module Data.Bitstream.C(
    Bitstream(..)
  , empty
  ) where

import Control.Monad.IO.Class
import Data.Bitstream.C.Raw
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

-- | Wrapper around C object to track buffers.
data Bitstream = Bitstream {
  bitstreamBuffer :: !(ForeignPtr CUChar)
, bitstreamWriter :: !(ForeignPtr BitstreamWriter)
}

-- | Allocate new bitstream writer buffer
empty :: MonadIO m
  => Int -- ^ Size in bytes
  -> m Bitstream
empty n = liftIO $ Bitstream
  <$> mallocForeignPtrBytes n
  <*> (newForeignPtr bitstream_writer_delete_ptr =<< bitstream_writer_new)
