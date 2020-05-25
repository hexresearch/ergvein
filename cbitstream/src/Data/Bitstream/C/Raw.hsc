module Data.Bitstream.C.Raw where

import Foreign.C.Types
import Foreign.Ptr

data BitstreamWriter
data BitstreamWriterBounds
data BitstreamReader

{-*
  * The writer.
  * -}

foreign import ccall safe "bitstream_writer_new"
  bitstream_writer_new :: IO (Ptr BitstreamWriter)

foreign import ccall safe "bitstream_writer_delete"
  bitstream_writer_delete :: Ptr BitstreamWriter -> IO ()

foreign import ccall safe "&bitstream_writer_delete"
  bitstream_writer_delete_ptr :: FunPtr (Ptr BitstreamWriter -> IO ())

foreign import ccall safe "bitstream_writer_init"
  bitstream_writer_init :: Ptr BitstreamWriter -> Ptr CUChar -> IO ()

foreign import ccall safe "bitstream_writer_update_buffer"
  bitstream_writer_update_buffer :: Ptr BitstreamWriter -> Ptr CUChar -> IO ()

foreign import ccall safe "bitstream_writer_size_in_bits"
  bitstream_writer_size_in_bits :: Ptr BitstreamWriter -> IO CInt

foreign import ccall safe "bitstream_writer_size_in_bytes"
  bitstream_writer_size_in_bytes :: Ptr BitstreamWriter -> IO CInt

-- Write bits to the stream. Clears each byte before bits are
-- written.

foreign import ccall safe "bitstream_writer_write_bit"
  bitstream_writer_write_bit :: Ptr BitstreamWriter -> CInt -> IO ()

foreign import ccall safe "bitstream_writer_write_bytes"
  bitstream_writer_write_bytes :: Ptr BitstreamWriter -> Ptr CUChar -> CInt -> IO ()

foreign import ccall safe "bitstream_writer_write_u8"
  bitstream_writer_write_u8 :: Ptr BitstreamWriter -> CUChar -> IO ()

foreign import ccall safe "bitstream_writer_write_u16"
  bitstream_writer_write_u16 :: Ptr BitstreamWriter -> CUShort -> IO ()

foreign import ccall safe "bitstream_writer_write_u32"
  bitstream_writer_write_u32 :: Ptr BitstreamWriter -> CUInt -> IO ()

foreign import ccall safe "bitstream_writer_write_u64"
  bitstream_writer_write_u64 :: Ptr BitstreamWriter -> CULong -> IO ()

-- Upper unused bits must be zero.

foreign import ccall safe "bitstream_writer_write_u64_bits"
  bitstream_writer_write_u64_bits :: Ptr BitstreamWriter -> CULong -> CInt -> IO ()

foreign import ccall safe "bitstream_writer_write_repeated_bit"
  bitstream_writer_write_repeated_bit :: Ptr BitstreamWriter -> CInt -> CInt -> IO ()

foreign import ccall safe "bitstream_writer_write_repeated_u8"
  bitstream_writer_write_repeated_u8 :: Ptr BitstreamWriter -> CUChar -> CInt -> IO ()

-- Insert bits into the stream. Leaves all other bits unmodified.

foreign import ccall safe "bitstream_writer_insert_bit"
  bitstream_writer_insert_bit :: Ptr BitstreamWriter -> CInt -> IO ()

foreign import ccall safe "bitstream_writer_insert_bytes"
  bitstream_writer_insert_bytes :: Ptr BitstreamWriter -> Ptr CUChar -> CInt -> IO ()

foreign import ccall safe "bitstream_writer_insert_u8"
  bitstream_writer_insert_u8 :: Ptr BitstreamWriter -> CUChar -> IO ()

foreign import ccall safe "bitstream_writer_insert_u16"
  bitstream_writer_insert_u16 :: Ptr BitstreamWriter -> CUShort -> IO ()

foreign import ccall safe "bitstream_writer_insert_u32"
  bitstream_writer_insert_u32 :: Ptr BitstreamWriter -> CUInt -> IO ()

foreign import ccall safe "bitstream_writer_insert_u64"
  bitstream_writer_insert_u64 :: Ptr BitstreamWriter -> CULong -> IO ()

foreign import ccall safe "bitstream_writer_insert_u64_bits"
  bitstream_writer_insert_u64_bits :: Ptr BitstreamWriter -> CULong -> CInt -> IO ()

-- | Move write position. Seeking backwards makes the written size
-- smaller. Use write with care after seek, as seek does not clear
-- bytes.
foreign import ccall safe "bitstream_writer_seek"
  bitstream_writer_seek :: Ptr BitstreamWriter -> CUInt -> IO ()

foreign import ccall safe "bitstream_writer_bounds_new"
  bitstream_writer_bounds_new :: IO (Ptr BitstreamWriterBounds)

foreign import ccall safe "bitstream_writer_bounds_delete"
  bitstream_writer_bounds_delete :: Ptr BitstreamWriterBounds -> IO ()

foreign import ccall "&bitstream_writer_bounds_delete"
  bitstream_writer_bounds_delete_ptr :: FunPtr (Ptr BitstreamWriterBounds -> IO ())

-- | Save-restore first and last bytes in given range, so write can be
-- used in given range.
foreign import ccall safe "bitstream_writer_bounds_save"
  bitstream_writer_bounds_save :: Ptr BitstreamWriterBounds -> Ptr BitstreamWriter -> CUInt -> CUInt -> IO ()

foreign import ccall safe "bitstream_writer_bounds_restore"
  bitstream_writer_bounds_restore :: Ptr BitstreamWriterBounds -> IO ()

{- *
   * The reader.
   * -}

foreign import ccall safe "bitstream_reader_new"
  bitstream_reader_new :: IO (Ptr BitstreamReader)

foreign import ccall safe "bitstream_reader_delete"
  bitstream_reader_delete :: Ptr BitstreamReader -> IO ()

foreign import ccall "&bitstream_reader_delete"
  bitstream_reader_delete_ptr :: FunPtr (Ptr BitstreamReader -> IO ())

foreign import ccall safe "bitstream_reader_update_buffer"
  bitstream_reader_update_buffer :: Ptr BitstreamReader -> Ptr CUChar -> IO ()

foreign import ccall safe "bitstream_reader_init"
  bitstream_reader_init :: Ptr BitstreamReader -> Ptr CUChar -> IO ()

-- Read bits from the stream.

foreign import ccall safe "bitstream_reader_read_bit"
  bitstream_reader_read_bit :: Ptr BitstreamReader -> IO CInt

foreign import ccall safe "bitstream_reader_read_bytes"
  bitstream_reader_read_bytes :: Ptr BitstreamReader -> Ptr CUChar -> CInt -> IO ()

foreign import ccall safe "bitstream_reader_read_u8"
  bitstream_reader_read_u8 :: Ptr BitstreamReader -> IO CUChar

foreign import ccall safe "bitstream_reader_read_u16"
  bitstream_reader_read_u16 :: Ptr BitstreamReader -> IO CUShort

foreign import ccall safe "bitstream_reader_read_u32"
  bitstream_reader_read_u32 :: Ptr BitstreamReader -> IO CUInt

foreign import ccall safe "bitstream_reader_read_u64"
  bitstream_reader_read_u64 :: Ptr BitstreamReader -> IO CULong

foreign import ccall safe "bitstream_reader_read_u64_bits"
  bitstream_reader_read_u64_bits :: Ptr BitstreamReader -> CInt -> IO CULong

-- | Move read position.
foreign import ccall safe "bitstream_reader_seek"
  bitstream_reader_seek :: Ptr BitstreamReader -> CInt -> IO ()

-- | Get read position.
foreign import ccall safe "bitstream_reader_tell"
  bitstream_reader_tell :: Ptr BitstreamReader -> IO CInt
