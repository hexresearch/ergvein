-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
module Ergvein.Types.Storage.Wallet
  (
    WalletStorage(..)
  , EncryptedWalletStorage(..)
  -- * Export lenses
  , storage'encryptedPrvStorage
  , storage'pubStorage
  , storage'walletName
  , encryptedStorage'ciphertext
  , encryptedStorage'salt
  , encryptedStorage'iv
  , encryptedStorage'eciesPoint
  , encryptedStorage'authTag
  ) where

import Control.Lens
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.ECC (Curve_X25519, Point, decodePoint)
import Crypto.Error
import Data.ByteArray
import Data.ByteString (ByteString)
import Data.Proxy
import Data.SafeCopy
import Data.Serialize
import Data.Text (Text,)

import Ergvein.Types.Storage.Public
import Ergvein.Types.Storage.Private

-- ====================================================================
--      WalletStorage. Not encrypted
-- ====================================================================

data WalletStorage = WalletStorage {
    _storage'encryptedPrvStorage :: !EncryptedPrvStorage
  , _storage'pubStorage          :: !PubStorage
  , _storage'walletName          :: !Text
  }

instance SafeCopy WalletStorage where
  version = 1
  putCopy (WalletStorage e p w) = contain $ do
    safePut e >> safePut p >> put w
  getCopy = contain $ WalletStorage <$> safeGet <*> safeGet <*> get

-- ====================================================================
--      EncryptedWalletStorage
-- ====================================================================

data EncryptedWalletStorage = EncryptedWalletStorage {
    _encryptedStorage'ciphertext :: !ByteString
  , _encryptedStorage'salt       :: !ByteString
  , _encryptedStorage'iv         :: !(IV AES256)
  , _encryptedStorage'eciesPoint :: !(Point Curve_X25519)
  , _encryptedStorage'authTag    :: !AuthTag
  }

instance SafeCopy EncryptedWalletStorage where
  version = 1
  putCopy EncryptedWalletStorage{..} = contain $ do
    safePut _encryptedStorage'ciphertext
    safePut _encryptedStorage'salt
    safePut _encryptedStorage'iv
    safePut (convert _encryptedStorage'eciesPoint :: ByteString)
    safePut (convert _encryptedStorage'authTag :: ByteString)
  getCopy = contain $ do
    cip <- safeGet
    salt <- safeGet
    iv <- safeGet
    eciesbs :: ByteString <- safeGet
    tagbs :: ByteString <- safeGet
    let authTag = AuthTag (convert tagbs :: Bytes)
        curve = Proxy :: Proxy Curve_X25519
    case decodePoint curve eciesbs of
      CryptoFailed _ -> fail "failed to read eciesPoint"
      CryptoPassed eciesPoint -> pure $ EncryptedWalletStorage cip salt iv eciesPoint authTag

-- ====================================================================
-- These instances are required only for the current version
-- ====================================================================
makeLenses ''WalletStorage
makeLenses ''EncryptedWalletStorage

instance Eq WalletStorage where
  a == b = _storage'walletName a == _storage'walletName b
