module Ergvein.Interfaces.Ergo.Scorex.Util.Serialization.VLQLengthPrefixed where

import Data.Int
import Data.Serialize as S (Serialize (..), decode, encode, get, put)

import qualified Data.ByteString as BS

newtype VLQLengthPrefixed a = VLQLengthPrefixed a
newtype VLQInt32 = VLQInt32 Int32

-- | @note Uses ZigZag encoding. Should be used to decode '''only''' a value that was previously
--       encoded with [[VLQByteBufferWriter.putInt]].
-- @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
instance Serialize VLQInt32 where
  put = undefined
  get = undefined

-- val headerBytes = obj.header.bytes
-- w.putInt(headerBytes.length)
-- w.putBytes(headerBytes)

instance Serialize a => Serialize (VLQLengthPrefixed [a]) where
  put b = do
      undefined
  get = do
      -- VLQInt32 n <- get
      undefined

instance Serialize (VLQLengthPrefixed BS.ByteString) where
  put b = do
      undefined
  get = do
      -- VLQInt32 n <- get
      undefined
