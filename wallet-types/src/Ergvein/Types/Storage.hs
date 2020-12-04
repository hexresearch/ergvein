{-# LANGUAGE ScopedTypeVariables #-}
module Ergvein.Types.Storage where

import Control.Lens (makeLenses)
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.ECC (Curve_X25519, Point, decodePoint)
import Crypto.Error
import Data.ByteArray (convert, Bytes)
import Data.ByteString (ByteString)
import Data.Proxy
import Data.SafeCopy
import Data.Text
import Data.Vector (Vector)
import Data.Word
import Network.Haskoin.Keys

import qualified Data.Map.Strict as M
import qualified Data.Serialize as SE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Haskoin.Block as HB

import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys
import Ergvein.Types.Orphanage ()
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo

type WalletName = Text

type Password = Text

data CurrencyPrvStorage = CurrencyPrvStorage {
    _currencyPrvStorage'prvKeystore :: !PrvKeystore
  , _currencyPrvStorage'path        :: !(Maybe DerivPrefix)
  } deriving (Eq, Show, Read)

makeLenses ''CurrencyPrvStorage

instance SafeCopy CurrencyPrvStorage where
  putCopy (CurrencyPrvStorage ks p)= contain $ safePut ks >> safePut p
  getCopy = contain $ CurrencyPrvStorage <$> safeGet <*> safeGet

type CurrencyPrvStorages = M.Map Currency CurrencyPrvStorage

data PrvStorage = PrvStorage {
    _prvStorage'mnemonic            :: Mnemonic
  , _prvStorage'rootPrvKey          :: EgvRootXPrvKey
  , _prvStorage'currencyPrvStorages :: CurrencyPrvStorages
  , _prvStorage'pathPrefix          :: !(Maybe DerivPrefix)
  } deriving (Eq, Show, Read)

makeLenses ''PrvStorage

instance SafeCopy PrvStorage where
  putCopy PrvStorage{..} = contain $ do
    SE.put $ T.unpack _prvStorage'mnemonic
    SE.put _prvStorage'rootPrvKey
    safePut _prvStorage'currencyPrvStorages
    safePut _prvStorage'pathPrefix
  getCopy = contain $ (PrvStorage . T.pack) <$> SE.get <*> SE.get <*> safeGet <*> safeGet

data EncryptedPrvStorage = EncryptedPrvStorage {
    _encryptedPrvStorage'ciphertext :: ByteString
  , _encryptedPrvStorage'salt       :: ByteString
  , _encryptedPrvStorage'iv         :: IV AES256
  }

makeLenses ''EncryptedPrvStorage

instance SafeCopy EncryptedPrvStorage where
  putCopy EncryptedPrvStorage{..} = contain $ do
    safePut _encryptedPrvStorage'ciphertext
    safePut _encryptedPrvStorage'salt
    safePut _encryptedPrvStorage'iv
  getCopy = contain $ EncryptedPrvStorage <$> safeGet <*> safeGet <*> safeGet

data CurrencyPubStorage = CurrencyPubStorage {
    _currencyPubStorage'pubKeystore   :: !PubKeystore
  , _currencyPubStorage'path          :: !(Maybe DerivPrefix)
  , _currencyPubStorage'transactions  :: !(M.Map TxId EgvTx)
  , _currencyPubStorage'utxos         :: !BtcUtxoSet              -- ^ TODO: Change to a generalized one, after we switch to DMaps
  , _currencyPubStorage'headers       :: !(M.Map HB.BlockHash HB.BlockHeader)
  , _currencyPubStorage'outgoing      :: !(S.Set TxId)
  , _currencyPubStorage'headerSeq     :: !(Word32, V.Vector (HB.BlockHeight, HB.BlockHash))
  , _currencyPubStorage'scannedHeight :: !BlockHeight
  , _currencyPubStorage'chainHeight   :: !BlockHeight
  } deriving (Eq, Show, Read)

makeLenses ''CurrencyPubStorage

instance SafeCopy CurrencyPubStorage where
  putCopy CurrencyPubStorage{..} = contain $ do
    safePut _currencyPubStorage'pubKeystore
    safePut _currencyPubStorage'path
    safePut _currencyPubStorage'transactions
    SE.put _currencyPubStorage'utxos
    SE.put _currencyPubStorage'headers
    SE.put _currencyPubStorage'outgoing
    SE.put _currencyPubStorage'headerSeq
    SE.put _currencyPubStorage'scannedHeight
    SE.put _currencyPubStorage'chainHeight
  getCopy = contain $ CurrencyPubStorage
    <$> safeGet <*> safeGet <*> safeGet <*> SE.get
    <*> SE.get <*> SE.get <*> SE.get <*> SE.get <*> SE.get

type CurrencyPubStorages = M.Map Currency CurrencyPubStorage

data PubStorage = PubStorage {
    _pubStorage'rootPubKey          :: !EgvRootXPubKey
  , _pubStorage'currencyPubStorages :: !CurrencyPubStorages
  , _pubStorage'activeCurrencies    :: [Currency]
  , _pubStorage'restoring           :: !Bool -- ^ Flag to track unfinished process of restoration
  , _pubStorage'pathPrefix          :: !(Maybe DerivPrefix)
  } deriving (Eq, Show, Read)

makeLenses ''PubStorage

instance SafeCopy PubStorage where
  putCopy PubStorage{..} = contain $ do
    SE.put _pubStorage'rootPubKey
    safePut _pubStorage'currencyPubStorages
    safePut _pubStorage'activeCurrencies
    SE.put _pubStorage'restoring
    SE.put _pubStorage'pathPrefix
  getCopy = contain $ PubStorage <$> SE.get <*> safeGet <*> safeGet <*> SE.get <*> SE.get

-- | Get pub storage keys
pubStorageKeys :: Currency -> KeyPurpose -> PubStorage -> Vector EgvXPubKey
pubStorageKeys c kp = fmap pubKeyBox'key . maybe mempty keys . fmap _currencyPubStorage'pubKeystore . M.lookup c . _pubStorage'currencyPubStorages
  where
    keys = case kp of
      External -> pubKeystore'external
      Internal -> pubKeystore'internal

pubStoragePubMaster :: Currency -> PubStorage -> Maybe EgvXPubKey
pubStoragePubMaster c = fmap pubKeystore'master . pubStorageKeyStorage c

pubStorageLastUnusedKey :: Currency -> KeyPurpose -> PubStorage -> Maybe (Int, EgvPubKeyBox)
pubStorageLastUnusedKey c kp ps = getLastUnusedKey kp . _currencyPubStorage'pubKeystore =<< M.lookup c (_pubStorage'currencyPubStorages ps)

pubStorageKeyStorage :: Currency -> PubStorage -> Maybe PubKeystore
pubStorageKeyStorage c = fmap _currencyPubStorage'pubKeystore . M.lookup c . _pubStorage'currencyPubStorages

pubStorageSetKeyStorage :: Currency -> PubKeystore -> PubStorage -> PubStorage
pubStorageSetKeyStorage c ks ps = ps {
    _pubStorage'currencyPubStorages = M.adjust f c $ _pubStorage'currencyPubStorages ps
  }
  where
    f cps = cps {
        _currencyPubStorage'pubKeystore = ks
      }

modifyCurrStorage :: Currency -> (CurrencyPubStorage  -> CurrencyPubStorage) -> PubStorage -> PubStorage
modifyCurrStorage c f ps = ps {
    _pubStorage'currencyPubStorages = M.adjust f c $ _pubStorage'currencyPubStorages ps
  }

pubStorageTxs :: Currency -> PubStorage -> Maybe (M.Map TxId EgvTx)
pubStorageTxs c = fmap _currencyPubStorage'transactions . M.lookup c . _pubStorage'currencyPubStorages

data WalletStorage = WalletStorage {
    _storage'encryptedPrvStorage :: EncryptedPrvStorage
  , _storage'pubStorage          :: PubStorage
  , _storage'walletName          :: Text
  }

makeLenses ''WalletStorage

instance Eq WalletStorage where
  a == b = _storage'walletName a == _storage'walletName b

instance SafeCopy WalletStorage where
  putCopy (WalletStorage e p w) = contain $ do
    safePut e
    safePut p
    SE.put w
  getCopy = contain $ do
    e <- safeGet
    p <- safeGet
    w <- SE.get
    pure $ WalletStorage e p w

data EncryptedWalletStorage = EncryptedWalletStorage {
    _encryptedStorage'ciphertext :: ByteString
  , _encryptedStorage'salt       :: ByteString
  , _encryptedStorage'iv         :: IV AES256
  , _encryptedStorage'eciesPoint :: Point Curve_X25519
  , _encryptedStorage'authTag    :: AuthTag
  }

makeLenses ''EncryptedWalletStorage

instance SafeCopy EncryptedWalletStorage where
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
