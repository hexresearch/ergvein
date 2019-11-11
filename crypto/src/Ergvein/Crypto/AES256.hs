module Ergvein.Crypto.AES256(
    encrypt
  , decrypt
  , encryptWithAEAD
  , decryptWithAEAD
  , fastPBKDF2_SHA256
  , defaultPBKDF2Params
  , defaultPBKDF2SaltLength
  , defaultAuthTagLength
  , genRandomSalt
  , genRandomIV
  , AES256
  , AEADMode(..)
  , Key(..)
  , IV
  , makeIV
  , AuthTag(..)
  , MonadRandom
  ) where

import Data.ByteArray (ByteArray, ByteArrayAccess)
import Data.ByteString (ByteString)
import Crypto.KDF.PBKDF2 (Parameters(..), fastPBKDF2_SHA256)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
import Crypto.Error (CryptoFailable(..), CryptoError(..))
import Crypto.Random.Types (MonadRandom, getRandomBytes)

data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

defaultPBKDF2Params :: Parameters
defaultPBKDF2Params = Parameters {
    iterCounts = 100000
  , outputLength = 32
}

defaultPBKDF2SaltLength :: Int
defaultPBKDF2SaltLength = 32

defaultAuthTagLength :: Int
defaultAuthTagLength = 16

-- | Generate a random salt with length equal to 'defaultPBKDF2SaltLength'
genRandomSalt :: (MonadRandom m, ByteArray a) => m a
genRandomSalt = getRandomBytes defaultPBKDF2SaltLength

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall m c. (MonadRandom m, BlockCipher c) => c -> m (Maybe (IV c))
genRandomIV _ = do
  bytes :: ByteString <- getRandomBytes $ blockSize (undefined :: c)
  return $ makeIV bytes

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a

encrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encrypt secretKey iv msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ ctrCombine c iv msg

decrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
decrypt = encrypt

-- | Initialize an AEAD block cipher
initAEADCipher :: (BlockCipher c, ByteArray a)
  => AEADMode
  -> Key c a
  -> IV c
  -> Either CryptoError (AEAD c)
initAEADCipher mode secretKey iv =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> case aeadInit mode c iv of
      CryptoFailed e -> Left e
      CryptoPassed a -> Right a

encryptWithAEAD :: (BlockCipher c, ByteArray a, ByteArrayAccess aad)
  => AEADMode
  -> Key c a
  -> IV c
  -> aad
  -> a
  -> Int
  -> Either CryptoError (AuthTag, a)
encryptWithAEAD mode secretKey iv header msg tagLength =
  case initAEADCipher mode secretKey iv of
    Left e -> Left e
    Right context -> Right $ aeadSimpleEncrypt context header msg tagLength

decryptWithAEAD :: (BlockCipher c, ByteArray a, ByteArrayAccess aad)
  => AEADMode
  -> Key c a
  -> IV c
  -> aad
  -> a
  -> AuthTag
  -> Maybe a
decryptWithAEAD mode secretKey iv header msg tag =
  case initAEADCipher mode secretKey iv of
    Left e -> error $ show e
    Right context -> aeadSimpleDecrypt context header msg tag
