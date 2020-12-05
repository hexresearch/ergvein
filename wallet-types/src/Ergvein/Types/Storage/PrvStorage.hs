-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
-- Read the README.md to learn now to work with migrations
module Ergvein.Types.Storage.PrvStorage
  (
    PrvStorage(..)
  , EncryptedPrvStorage(..)
  -- * Export lenses
  , prvStorage'mnemonic
  , prvStorage'rootPrvKey
  , prvStorage'currencyPrvStorages
  , prvStorage'pathPrefix
  , encryptedPrvStorage'ciphertext
  , encryptedPrvStorage'salt
  , encryptedPrvStorage'iv
  ) where

import Control.Lens
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Data.ByteString (ByteString)
import Data.SafeCopy
import Data.Serialize
import Data.Text (pack, unpack)

import Ergvein.Types.Derive
import Ergvein.Types.Keys.Prim
import Ergvein.Types.Storage.CurrencyPrvStorage
import Ergvein.Crypto.Keys

-- ====================================================================
--      PrvStorage. Not encrypted
-- ====================================================================

data PrvStorage = PrvStorage {
    _prvStorage'mnemonic            :: Mnemonic
  , _prvStorage'rootPrvKey          :: EgvRootXPrvKey
  , _prvStorage'currencyPrvStorages :: CurrencyPrvStorages
  , _prvStorage'pathPrefix          :: !(Maybe DerivPrefix)
  } deriving (Eq, Show, Read)

instance SafeCopy PrvStorage where
  version = 1
  putCopy PrvStorage{..} = contain $ do
    put $ unpack _prvStorage'mnemonic
    put _prvStorage'rootPrvKey
    safePut _prvStorage'currencyPrvStorages
    safePut _prvStorage'pathPrefix
  getCopy = contain $ (PrvStorage . pack) <$> get <*> get <*> safeGet <*> safeGet

-- ====================================================================
--      EncryptedPrvStorage
-- ====================================================================

data EncryptedPrvStorage = EncryptedPrvStorage {
    _encryptedPrvStorage'ciphertext :: ByteString
  , _encryptedPrvStorage'salt       :: ByteString
  , _encryptedPrvStorage'iv         :: IV AES256
  }

instance SafeCopy EncryptedPrvStorage where
  version = 1
  putCopy EncryptedPrvStorage{..} = contain $ do
    safePut _encryptedPrvStorage'ciphertext
    safePut _encryptedPrvStorage'salt
    safePut _encryptedPrvStorage'iv
  getCopy = contain $ EncryptedPrvStorage <$> safeGet <*> safeGet <*> safeGet

-- ====================================================================
-- These instances are required only for the current version
-- ====================================================================
makeLenses ''PrvStorage
makeLenses ''EncryptedPrvStorage
