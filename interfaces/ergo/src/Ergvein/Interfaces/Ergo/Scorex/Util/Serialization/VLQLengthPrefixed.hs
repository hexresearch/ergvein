module Ergvein.Interfaces.Ergo.Scorex.Util.Serialization.VLQLengthPrefixed where

import Control.Monad
import Data.Int
import Data.Serialize as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get as S
import Data.Word

import qualified Data.ByteString as BS

newtype VLQLengthPrefixed a = VLQLengthPrefixed a
newtype VLQInt32 = VLQInt32 { unVLQInt32 :: Int32 }
newtype VLQWord64 = VLQWord64 { unVLQWord64 :: Word64 }

-- | @note Uses ZigZag encoding. Should be used to decode '''only''' a value that was previously
--       encoded with [[VLQByteBufferWriter.putInt]].
-- @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
instance Serialize VLQInt32 where
  -- @inline override def putInt(x: Int): this.type = putULong(encodeZigZagInt(x))
  put = put . VLQWord64 . toZigZag . unVLQInt32

  -- @inline override def getInt(): Int = { decodeZigZagInt(getULong().toInt) }
  get = VLQInt32 . fromZigZag . unVLQWord64 <$> get

toZigZag :: Int32 -> Word64
toZigZag = undefined

fromZigZag :: Word64 -> Int32
fromZigZag = undefined

instance Serialize VLQWord64 where
  put = undefined
  get = undefined

instance Serialize a => Serialize (VLQLengthPrefixed [a]) where
  put (VLQLengthPrefixed as) = do
      -- w.putInt(obj.interlinks.size)
      put . VLQInt32 . fromIntegral . length $ as
      -- obj.interlinks.foreach(x => w.putBytes(idToBytes(x)))
      mapM_ put as
  get = do
      n <- fromIntegral . unVLQInt32 <$> get
      VLQLengthPrefixed <$> replicateM n get

instance Serialize (VLQLengthPrefixed BS.ByteString) where
  put (VLQLengthPrefixed bs) = do
      -- w.putInt(headerBytes.length)
      put . VLQInt32 . fromIntegral . BS.length $ bs
      -- w.putBytes(headerBytes)
      put bs
  get = do
      n <- fromIntegral . unVLQInt32 <$> get
      VLQLengthPrefixed <$> S.getBytes n
