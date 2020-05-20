{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Bitstream.C(
    Bitstream(..)
  , empty
  , null
  , length
  , fromByteString
  , unsafeFromByteString
  , toByteString
  , unsafeToByteString
  , pack
  , WriteBits(..)
  , writeNBits
  , ReadBits(..)
  , readByteString
  , readNBits
  ) where

import Control.Monad.IO.Class
import Data.Bitstream.C.Raw
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Generics
import Prelude hiding (null, length)
import Data.Bits

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Prelude as P

-- | Wrapper around C object to track buffers.
data Bitstream = Bitstream {
  bitstreamSize   :: !Int -- ^ Size of allocated buffer
, bitstreamBuffer :: !(ForeignPtr CUChar) -- ^ Buffer for bits
, bitstreamWriter :: !(ForeignPtr BitstreamWriter) -- ^ Control structure for writing bits
, bitstreamReader :: !(ForeignPtr BitstreamReader) -- ^ Control structure for reading bits
} deriving (Show, Generic)

-- | Allocate new bitstream writer buffer
empty :: MonadIO m
  => Int -- ^ Size in bytes
  -> m Bitstream
empty n = liftIO $ Bitstream
  <$> pure n
  <*> mallocForeignPtrBytes n
  <*> (newForeignPtr bitstream_writer_delete_ptr =<< bitstream_writer_new)
  <*> (newForeignPtr bitstream_reader_delete_ptr =<< bitstream_reader_new)
{-# INLINE empty #-}

-- | Return 'True' if given stream is empty
null :: MonadIO m  => Bitstream -> m Bool
null Bitstream{..} = liftIO $ withForeignPtr bitstreamWriter $ \ptr -> do
  n <- bitstream_writer_size_in_bytes ptr
  pure $ n == 0
{-# INLINE null #-}

-- | Get filled size in bytes of bitstream.
length :: MonadIO m => Bitstream -> m Int
length Bitstream{..} = liftIO $ fmap fromIntegral $ withForeignPtr bitstreamWriter bitstream_writer_size_in_bytes
{-# INLINE length #-}

-- | Convert bytes to stream of bits. O(n)
fromByteString :: MonadIO m => ByteString -> m Bitstream
fromByteString bs = liftIO $ do
  sw <- empty (BS.length bs)
  BS.unsafeUseAsCStringLen bs $ \(ptr, l) -> withForeignPtr (bitstreamWriter sw) $ \wp -> do
    bitstream_writer_write_bytes wp (castPtr ptr) (fromIntegral l)
  pure sw
{-# INLINABLE fromByteString #-}

-- | Convert bytes to stream of bits. O(1). The function performs zero copying and
-- if source bytestring is deallocated or modified the resulted bitstream is also
-- afftected.
unsafeFromByteString :: MonadIO m => ByteString -> m Bitstream
unsafeFromByteString  bs = liftIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, l) -> do
  wpw <- newForeignPtr bitstream_writer_delete_ptr =<< bitstream_writer_new
  wpr <- newForeignPtr bitstream_reader_delete_ptr =<< bitstream_reader_new
  fptr <- newForeignPtr_ (castPtr ptr)
  pure $ Bitstream (fromIntegral l) fptr wpw wpr
{-# INLINABLE unsafeFromByteString #-}

-- | Convert stream of bits to bytestring. O(n)
toByteString :: MonadIO m => Bitstream -> m ByteString
toByteString sw = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp -> do
  l <- bitstream_writer_size_in_bytes wp
  BS.packCStringLen (castPtr wp, fromIntegral l)
{-# INLINABLE toByteString #-}

-- | Convert stream of bits to bytestring. O(1). Doesn't copy internal buffer,
-- so if bitstream deallocated and modified the resulted bytestring is affected.
unsafeToByteString :: MonadIO m => Bitstream -> m ByteString
unsafeToByteString sw = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp -> do
  l <- bitstream_writer_size_in_bytes wp
  BS.unsafePackCStringLen (castPtr wp, fromIntegral l)
{-# INLINABLE unsafeToByteString #-}

-- | Pack given list of bits into bitstream.
pack :: MonadIO m => [Bool] -> m Bitstream
pack bs = liftIO $ do
  let n = ceiling $ (fromIntegral (P.length bs) :: Double) / 8
  sw <- empty n
  withForeignPtr (bitstreamWriter sw) $ \wp ->
    flip traverse_ bs $ \i -> bitstream_writer_write_bit wp $ if i then 1 else 0
  pure sw
{-# INLINEABLE pack #-}

class WriteBits a where
  -- | Write down value to bitstream
  writeBits :: MonadIO m => Bitstream -> a -> m ()

instance WriteBits Bool where
  writeBits sw v = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
    bitstream_writer_write_bit wp (if v then 1 else 0)
  {-# INLINABLE writeBits #-}

instance WriteBits ByteString where
  writeBits sw bs = liftIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, l) ->
    withForeignPtr (bitstreamWriter sw) $ \wp ->
      bitstream_writer_write_bytes wp (castPtr ptr) (fromIntegral l)
  {-# INLINABLE writeBits #-}

instance WriteBits Word8 where
  writeBits sw v = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
    bitstream_writer_write_u8 wp (fromIntegral v)
  {-# INLINABLE writeBits #-}

instance WriteBits Word16 where
  writeBits sw v = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
    bitstream_writer_write_u16 wp (fromIntegral v)
  {-# INLINABLE writeBits #-}

instance WriteBits Word32 where
  writeBits sw v = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
    bitstream_writer_write_u32 wp (fromIntegral v)
  {-# INLINABLE writeBits #-}

instance WriteBits Word64 where
  writeBits sw v = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
    bitstream_writer_write_u64 wp (fromIntegral v)
  {-# INLINABLE writeBits #-}

-- | Write down only N lower bits from given word.
writeNBits :: MonadIO m => Bitstream -> Word64 -> Int -> m ()
writeNBits sw v i = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp -> do
  let i' = 64-i
      v' = shiftR (shiftL v i') i'
  bitstream_writer_write_u64_bits wp (fromIntegral v') (fromIntegral i)
{-# INLINABLE writeNBits #-}

class ReadBits a where
  -- | Read value from bitstream
  readBits :: MonadIO m => Bitstream -> m a

instance ReadBits Bool where
  readBits sr = liftIO $ withForeignPtr (bitstreamReader sr) $ \rp -> do
    v <- bitstream_reader_read_bit rp
    pure $ if v > 0 then False else True
  {-# INLINABLE readBits #-}

instance ReadBits Word8 where
  readBits sr = liftIO $ withForeignPtr (bitstreamReader sr) $
    fmap fromIntegral . bitstream_reader_read_u8
  {-# INLINABLE readBits #-}

instance ReadBits Word16 where
  readBits sr = liftIO $ withForeignPtr (bitstreamReader sr) $
    fmap fromIntegral . bitstream_reader_read_u16
  {-# INLINABLE readBits #-}

instance ReadBits Word32 where
  readBits sr = liftIO $ withForeignPtr (bitstreamReader sr) $
    fmap fromIntegral . bitstream_reader_read_u32
  {-# INLINABLE readBits #-}

instance ReadBits Word64 where
  readBits sr = liftIO $ withForeignPtr (bitstreamReader sr) $
    fmap fromIntegral . bitstream_reader_read_u64
  {-# INLINABLE readBits #-}

-- | Read given amount of bytes from the stream
readByteString :: MonadIO m => Bitstream -> Int -> m ByteString
readByteString sr i = liftIO $ do
  let bs = BS.replicate i 0
  BS.unsafeUseAsCString bs $ \bsptr ->
    withForeignPtr (bitstreamReader sr) $ \rp ->
      bitstream_reader_read_bytes rp (castPtr bsptr) (fromIntegral i)
  pure bs
{-# INLINABLE readByteString #-}

-- | Read given amount of bits and convert them into word considering them as low bits.
readNBits :: MonadIO m => Bitstream -> Int -> m Word64
readNBits sr i = liftIO $ withForeignPtr (bitstreamReader sr) $ \rp ->
  fmap fromIntegral $ bitstream_reader_read_u64_bits rp (fromIntegral i)
{-# INLINABLE readNBits #-}
