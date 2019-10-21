module Ergvein.Crypto.AES256(
    encrypt
  , decrypt
  , fastPBKDF2_SHA256
  , defaultPBKDF2Params
  , defaultPBKDF2SaltLength
  , genRandomSalt
  , genRandomIV
  , AES256
  , Key(..)
  , IV
  , makeIV
  ) where

import           Data.ByteArray (ByteArray)
import           Data.ByteString (ByteString)
import           Crypto.KDF.PBKDF2 (Parameters(..), fastPBKDF2_SHA256)
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import           Crypto.Error (CryptoFailable(..), CryptoError(..))
import qualified Crypto.Random.Types as CRT

-- | Not required, but most general implementation
data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

defaultPBKDF2Params :: Parameters
defaultPBKDF2Params = Parameters {
    iterCounts = 100000
  , outputLength = 32
}

defaultPBKDF2SaltLength :: Int
defaultPBKDF2SaltLength = 32

-- | Generate a random salt with length equal to 'defaultPBKDF2SaltLength'
genRandomSalt :: (CRT.MonadRandom m, ByteArray a) => m a
genRandomSalt = CRT.getRandomBytes defaultPBKDF2SaltLength

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall m c. (CRT.MonadRandom m, BlockCipher c) => c -> m (Maybe (IV c))
genRandomIV _ = do
  bytes :: ByteString <- CRT.getRandomBytes $ blockSize (undefined :: c)
  return $ makeIV bytes

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a

encrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encrypt secretKey initIV msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ ctrCombine c initIV msg

decrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
decrypt = encrypt
