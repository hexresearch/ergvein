{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Bitstream.C(
    Bitstream(..)
  , empty
  , null
  , length
  , bits
  , drop
  , countWhile
  , resize
  , realloc
  , fromByteString
  , unsafeFromByteString
  , toByteString
  , unsafeToByteString
  , pack
  , unpack
  , WriteBits(..)
  , writeNBits
  , unsafeWriteNBits
  , replicateBits
  , unsafeReplicateBits
  , ReadBits(..)
  , readByteString
  , readNBits
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bitstream.C.Raw
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc hiding (realloc)
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.Generics
import Prelude hiding (null, length, drop)
import Data.Bits

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Prelude as P

import Debug.Trace

-- | Wrapper around C object to track buffers.
data Bitstream = Bitstream {
  bitstreamSize   :: !Int -- ^ Size of allocated buffer
, bitstreamBuffer :: !(ForeignPtr CUChar) -- ^ Buffer for bits
, bitstreamWriter :: !(ForeignPtr BitstreamWriter) -- ^ Control structure for writing bits
, bitstreamReader :: !(ForeignPtr BitstreamReader) -- ^ Control structure for reading bits
} deriving (Generic)

-- | Allocate new bitstream writer buffer
empty :: MonadIO m
  => Int -- ^ Size in bytes (preallocated)
  -> m Bitstream
empty n = liftIO $ do
  buff <- mallocForeignPtrBytes n
  wp <- bitstream_writer_new
  rp <- bitstream_reader_new
  bw <- newForeignPtr bitstream_writer_delete_ptr wp
  br <- newForeignPtr bitstream_reader_delete_ptr rp
  withForeignPtr buff $ \bp -> do
    bitstream_writer_init wp bp
    bitstream_reader_init rp bp
    pure $ Bitstream n buff bw br
{-# INLINE empty #-}

-- | Return 'True' if given stream is empty
null :: MonadIO m  => Bitstream -> m Bool
null bs = (0 ==) <$> bits bs
{-# INLINE null #-}

-- | Get filled size in bytes of bitstream.
length :: MonadIO m => Bitstream -> m Int
length Bitstream{..} = liftIO $ fmap fromIntegral $ do
  wn <- withForeignPtr bitstreamWriter bitstream_writer_size_in_bytes
  rn <- withForeignPtr bitstreamReader bitstream_reader_tell
  pure $ wn - (rn `div` 8)
{-# INLINE length #-}

-- | Get filled size in bits of bitstream.
bits :: MonadIO m => Bitstream -> m Int
bits Bitstream{..} = liftIO $ fmap fromIntegral $ do
  wn <- withForeignPtr bitstreamWriter bitstream_writer_size_in_bits
  rn <- withForeignPtr bitstreamReader bitstream_reader_tell
  pure $ wn - rn
{-# INLINE bits #-}

-- | Drop given amount of bits from bitstream.
drop :: MonadIO m => Int -> Bitstream -> m ()
drop i Bitstream{..} = liftIO $ withForeignPtr bitstreamReader $ \rp ->
  bitstream_reader_seek rp (fromIntegral i)
{-# INLINE drop #-}

-- | Count amount of bits that passes the predicate. Discard them.
countWhile :: MonadIO m => (Bool -> Bool) -> Bitstream -> m Int
countWhile f bs = go 0
  where
    go !acc = do
      v <- readBits bs
      if f v then go (acc+1) else pure acc
{-# INLINABLE countWhile #-}

-- | Reallocate internal buffer to the given size. If the size is smaller than
-- amount of bytes clamps to that
resize :: MonadIO m => Int -> Bitstream -> m Bitstream
resize n bs = liftIO $ withForeignPtr (bitstreamWriter bs) $ \wp -> do
  l <- fmap fromIntegral $ bitstream_writer_size_in_bytes wp
  let n' = if n < l then l else n
  newBuff <- mallocForeignPtrBytes n'
  withForeignPtr newBuff $ \newp -> withForeignPtr (bitstreamBuffer bs) $ \oldp -> do
    copyBytes newp oldp l
    withForeignPtr (bitstreamWriter bs) $ \wp -> bitstream_writer_update_buffer wp newp
    withForeignPtr (bitstreamReader bs) $ \rp -> bitstream_reader_update_buffer rp newp
  pure $ Bitstream n' newBuff (bitstreamWriter bs) (bitstreamReader bs)
{-# INLINABLE resize #-}

-- | Reallocate if needed to handle given amount of additiona bytes.
realloc :: MonadIO m => Int -> Bitstream -> m Bitstream
realloc n bs = liftIO $ withForeignPtr (bitstreamWriter bs) $ \wp -> do
  l <- fmap fromIntegral $ bitstream_writer_size_in_bytes wp
  if (bitstreamSize bs <= l + n)
    then resize (if l + n < 2*l then 2*l else l + n) bs
    else pure bs
{-# INLINEABLE realloc #-}

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
  wp <- bitstream_writer_new
  rp <- bitstream_reader_new
  bw <- newForeignPtr bitstream_writer_delete_ptr wp
  br <- newForeignPtr bitstream_reader_delete_ptr rp
  bitstream_writer_init wp $ castPtr ptr
  bitstream_reader_init rp $ castPtr ptr
  fptr <- newForeignPtr_ (castPtr ptr)
  pure $ Bitstream (fromIntegral l) fptr bw br
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
    flip traverse_ bs $ \i -> do
      bitstream_writer_write_bit wp $ if i then 1 else 0
  pure sw
{-# INLINEABLE pack #-}

-- | Read all bits from bistream into list
unpack :: MonadIO m => Bitstream -> m [Bool]
unpack bs = fmap reverse $ liftIO $ go []
  where
    go acc = do
      isnull <- null bs
      if isnull then pure acc else do
        v <- readBits bs
        go $ v : acc
{-# INLINABLE unpack #-}

class WriteBits a where
  -- | Write down value to bitstream with boundary check.
  writeBits :: MonadIO m => a -> Bitstream -> m Bitstream
  -- | Write down value to bitstream without boundary checks.
  unsafeWriteBits :: MonadIO m => a -> Bitstream -> m ()

instance WriteBits Bool where
  writeBits v sw = liftIO $ do
    li <- bits sw
    sw' <- if (div (li + 1) 8 > bitstreamSize sw) then realloc 1 sw else pure sw
    unsafeWriteBits v sw'
    pure sw'
  {-# INLINABLE writeBits #-}

  unsafeWriteBits v sw = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
      bitstream_writer_write_bit wp (if v then 1 else 0)
  {-# INLINABLE unsafeWriteBits #-}

instance WriteBits [Bool] where
  writeBits v sw = liftIO $ do
    let i = P.length v
        ib = ceiling $ (fromIntegral i :: Double) / 8
    li <- bits sw
    sw' <- if (div (li + i) 8 > bitstreamSize sw) then realloc ib sw else pure sw
    unsafeWriteBits v sw'
    pure sw'
  {-# INLINABLE writeBits #-}

  unsafeWriteBits as sw = traverse_ (flip unsafeWriteBits sw) as
  {-# INLINABLE unsafeWriteBits #-}

instance WriteBits ByteString where
  writeBits v sw = liftIO $ do
    sw' <- realloc (BS.length v) sw
    unsafeWriteBits v sw'
    pure sw'
  {-# INLINABLE writeBits #-}

  unsafeWriteBits bs sw = liftIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, l) ->
    withForeignPtr (bitstreamWriter sw) $ \wp ->
      bitstream_writer_write_bytes wp (castPtr ptr) (fromIntegral l)
  {-# INLINABLE unsafeWriteBits #-}

instance WriteBits Word8 where
  writeBits v sw = liftIO $ do
    sw' <- realloc 1 sw
    unsafeWriteBits v sw'
    pure sw'
  {-# INLINABLE writeBits #-}

  unsafeWriteBits v sw = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
    bitstream_writer_write_u8 wp (fromIntegral v)
  {-# INLINABLE unsafeWriteBits #-}

instance WriteBits Word16 where
  writeBits v sw = liftIO $ do
    sw' <- realloc 2 sw
    unsafeWriteBits v sw'
    pure sw'
  {-# INLINABLE writeBits #-}

  unsafeWriteBits v sw = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
    bitstream_writer_write_u16 wp (fromIntegral v)
  {-# INLINABLE unsafeWriteBits #-}

instance WriteBits Word32 where
  writeBits v sw = liftIO $ do
    sw' <- realloc 4 sw
    unsafeWriteBits v sw'
    pure sw'
  {-# INLINABLE writeBits #-}

  unsafeWriteBits v sw = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
    bitstream_writer_write_u32 wp (fromIntegral v)
  {-# INLINABLE unsafeWriteBits #-}

instance WriteBits Word64 where
  writeBits v sw = liftIO $ do
    sw' <- realloc 8 sw
    unsafeWriteBits v sw'
    pure sw'
  {-# INLINABLE writeBits #-}

  unsafeWriteBits v sw = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
    bitstream_writer_write_u64 wp (fromIntegral v)
  {-# INLINABLE unsafeWriteBits #-}

-- | Write down only N lower bits from given word.
writeNBits :: MonadIO m => Int -> Word64 -> Bitstream -> m Bitstream
writeNBits i v sw = liftIO $ do
  sw' <- realloc 8 sw
  unsafeWriteNBits i v sw'
  pure sw'
{-# INLINABLE writeNBits #-}

-- | Write down only N lower bits from given word. Doesn't check boundaries.
unsafeWriteNBits :: MonadIO m => Int -> Word64 -> Bitstream -> m ()
unsafeWriteNBits i v sw = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp -> do
  let i' = 64-i
      v' = shiftR (shiftL v i') i'
  bitstream_writer_write_u64_bits wp (fromIntegral v') (fromIntegral i)
{-# INLINABLE unsafeWriteNBits #-}

-- | Write down given amount of bits to stream.
replicateBits :: MonadIO m => Int -> Bool -> Bitstream -> m Bitstream
replicateBits i v sw = liftIO $ do
  let ib = ceiling $ (fromIntegral i :: Double) / 8.0
  sw' <- realloc ib sw
  unsafeReplicateBits i v sw'
  pure sw'
{-# INLINABLE replicateBits #-}

-- | Write down given amount of bits to stream. Doesn't check boundaries.
unsafeReplicateBits :: MonadIO m => Int -> Bool -> Bitstream -> m ()
unsafeReplicateBits i v sw = liftIO $ withForeignPtr (bitstreamWriter sw) $ \wp ->
  bitstream_writer_write_repeated_bit wp (if v then 1 else 0) (fromIntegral i)
{-# INLINABLE unsafeReplicateBits #-}

class ReadBits a where
  -- | Read value from bitstream
  readBits :: MonadIO m => Bitstream -> m a

instance ReadBits Bool where
  readBits sr = liftIO $ withForeignPtr (bitstreamReader sr) $ \rp -> do
    v <- bitstream_reader_read_bit rp
    pure $ if v > 0 then True else False
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
readByteString :: MonadIO m => Int -> Bitstream -> m ByteString
readByteString i sr = liftIO $ do
  let bs = BS.replicate i 0
  BS.unsafeUseAsCString bs $ \bsptr ->
    withForeignPtr (bitstreamReader sr) $ \rp ->
      bitstream_reader_read_bytes rp (castPtr bsptr) (fromIntegral i)
  pure bs
{-# INLINABLE readByteString #-}

-- | Read given amount of bits and convert them into word considering them as low bits.
readNBits :: MonadIO m => Int -> Bitstream -> m Word64
readNBits i sr = liftIO $ withForeignPtr (bitstreamReader sr) $ \rp ->
  fmap fromIntegral $ bitstream_reader_read_u64_bits rp (fromIntegral i)
{-# INLINABLE readNBits #-}
