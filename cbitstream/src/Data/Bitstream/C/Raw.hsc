module Data.Bitstream.C.Raw where

import Foreign.C.Types
import Foreign.Ptr

newtype BitstreamWriter = BitstreamWriter { unBitstreamWriter :: Ptr BitstreamWriter }

newtype BitstreamWriterBounds = BitstreamWriterBounds { unBitstreamWriterBounds :: Ptr BitstreamWriterBounds }

newtype BitstreamReader = BitstreamReader { unBitstreamReader :: Ptr BitstreamReader }

{-*
  * The writer.
  * -}

foreign import ccall unsafe "bitstream_writer_init"
  bitstream_writer_init :: BitstreamWriter -> Ptr CUChar -> IO ()

foreign import ccall unsafe "bitstream_writer_size_in_bits"
  bitstream_writer_size_in_bits :: BitstreamWriter -> IO CInt

foreign import ccall unsafe "bitstream_writer_size_in_bytes"
  bitstream_writer_size_in_bytes :: BitstreamWriter -> IO CInt

-- Write bits to the stream. Clears each byte before bits are
-- written.

foreign import ccall unsafe "bitstream_writer_write_bit"
  bitstream_writer_write_bit :: BitstreamWriter -> CInt -> IO ()

foreign import ccall unsafe "bitstream_writer_write_bytes"
  bitstream_writer_write_bytes :: BitstreamWriter -> Ptr CUChar -> CInt -> IO ()

foreign import ccall unsafe "bitstream_writer_write_u8"
  bitstream_writer_write_u8 :: BitstreamWriter -> CUChar -> IO ()

foreign import ccall unsafe "bitstream_writer_write_u16"
  bitstream_writer_write_u16 :: BitstreamWriter -> CUShort -> IO ()

foreign import ccall unsafe "bitstream_writer_write_u32"
  bitstream_writer_write_u32 :: BitstreamWriter -> CUInt -> IO ()

foreign import ccall unsafe "bitstream_writer_write_u64"
  bitstream_writer_write_u64 :: BitstreamWriter -> CULong -> IO ()

-- Upper unused bits must be zero.

foreign import ccall unsafe "bitstream_writer_write_u64_bits"
  bitstream_writer_write_u64_bits :: BitstreamWriter -> CULong -> CInt -> IO ()

foreign import ccall unsafe "bitstream_writer_write_repeated_bit"
  bitstream_writer_write_repeated_bit :: BitstreamWriter -> CInt -> CInt -> IO ()

foreign import ccall unsafe "bitstream_writer_write_repeated_u8"
  bitstream_writer_write_repeated_u8 :: BitstreamWriter -> CUChar -> CInt -> IO ()

-- Insert bits into the stream. Leaves all other bits unmodified.

foreign import ccall unsafe "bitstream_writer_insert_bit"
  bitstream_writer_insert_bit :: BitstreamWriter -> CInt -> IO ()

foreign import ccall unsafe "bitstream_writer_insert_bytes"
  bitstream_writer_insert_bytes :: BitstreamWriter -> Ptr CUChar -> CInt -> IO ()

foreign import ccall unsafe "bitstream_writer_insert_u8"
  bitstream_writer_insert_u8 :: BitstreamWriter -> CUChar -> IO ()

foreign import ccall unsafe "bitstream_writer_insert_u16"
  bitstream_writer_insert_u16 :: BitstreamWriter -> CUShort -> IO ()

foreign import ccall unsafe "bitstream_writer_insert_u32"
  bitstream_writer_insert_u32 :: BitstreamWriter -> CUInt -> IO ()

foreign import ccall unsafe "bitstream_writer_insert_u64"
  bitstream_writer_insert_u64 :: BitstreamWriter -> CULong -> IO ()

foreign import ccall unsafe "bitstream_writer_insert_u64_bits"
  bitstream_writer_insert_u64_bits :: BitstreamWriter -> CULong -> CInt -> IO ()

-- | Move write position. Seeking backwards makes the written size
-- smaller. Use write with care after seek, as seek does not clear
-- bytes.
foreign import ccall unsafe "bitstream_writer_seek"
  bitstream_writer_seek :: BitstreamWriter -> CUInt -> IO ()

-- | Save-restore first and last bytes in given range, so write can be
-- used in given range.
foreign import ccall unsafe "bitstream_writer_bounds_save"
  bitstream_writer_bounds_save :: BitstreamWriterBounds -> BitstreamWriter -> CUInt -> CUInt -> IO ()

foreign import ccall unsafe "bitstream_writer_bounds_restore"
  bitstream_writer_bounds_restore :: BitstreamWriterBounds -> IO ()

{- *
   * The reader.
   * -}

foreign import ccall unsafe "bitstream_reader_init"
  bitstream_reader_init :: BitstreamReader -> Ptr CUChar -> IO ()

-- Read bits from the stream.

foreign import ccall unsafe "bitstream_reader_read_bit"
  bitstream_reader_read_bit :: BitstreamReader -> IO CInt

foreign import ccall unsafe "bitstream_reader_read_bytes"
  bitstream_reader_read_bytes :: BitstreamReader -> Ptr CUChar -> CInt -> IO ()

foreign import ccall unsafe "bitstream_reader_read_u8"
  bitstream_reader_read_u8 :: BitstreamReader -> IO CUChar

foreign import ccall unsafe "bitstream_reader_read_u16"
  bitstream_reader_read_u16 :: BitstreamReader -> IO CUShort

foreign import ccall unsafe "bitstream_reader_read_u32"
  bitstream_reader_read_u32 :: BitstreamReader -> IO CUInt

foreign import ccall unsafe "bitstream_reader_read_u64"
  bitstream_reader_read_u64 :: BitstreamReader -> IO CULong

foreign import ccall unsafe "bitstream_reader_read_u64_bits"
  bitstream_reader_read_u64_bits :: BitstreamReader -> CInt -> IO CULong
