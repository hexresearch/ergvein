module Ergvein.Types.Keys (
    XPrvKey(..)
  , XPubKey(..)
  , EgvRootXPrvKey(..)
  , EgvXPrvKey(..)
  , EgvXPubKey(..)
  , EgvPrvKeyсhain(..)
  , EgvPubKeyсhain(..)
  , KeyPurpose(..)
  , xPrvExport
  , xPrvImport
  , xPubExport
  , xPubImport
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types       (Parser)
import Data.Serialize         (get, put)
import Data.Serialize.Get     (Get, getWord32be, getWord8, runGet)
import Data.Serialize.Put     (Putter, putWord32be, putWord8, runPut)
import Ergvein.Aeson
import Ergvein.Crypto.Keys
import Ergvein.Crypto.Util
import Ergvein.Types.Currency
import Ergvein.Types.Network
import Text.Read              (readMaybe)

import qualified Data.IntMap.Strict as MI

-- | Parse a binary extended private key.
getXPrvKey :: EgvNetwork -> Get XPrvKey
getXPrvKey (EgvBtcNetwork net) = do
  ver <- getWord32be
  unless (ver == getExtSecretPrefix net) $ fail
      "Get: Invalid version for extended private key"
  XPrvKey <$> getWord8
          <*> getWord32be
          <*> getWord32be
          <*> get
          <*> getPadPrvKey
getXPrvKey (EgvErgNetwork net) = do
  ver <- getWord32be
  unless (ver == getErgExtSecretPrefix net) $ fail
      "Get: Invalid version for extended private key"
  XPrvKey <$> getWord8
          <*> getWord32be
          <*> getWord32be
          <*> get
          <*> getPadPrvKey

-- | Serialize an extended private key.
putXPrvKey :: EgvNetwork -> Putter XPrvKey
putXPrvKey (EgvBtcNetwork net) k = do
  putWord32be  $ getExtSecretPrefix net
  putWord8     $ xPrvDepth k
  putWord32be  $ xPrvParent k
  putWord32be  $ xPrvIndex k
  put          $ xPrvChain k
  putPadPrvKey $ xPrvKey k
putXPrvKey (EgvErgNetwork net) k = do
  putWord32be  $ getErgExtSecretPrefix net
  putWord8     $ xPrvDepth k
  putWord32be  $ xPrvParent k
  putWord32be  $ xPrvIndex k
  put          $ xPrvChain k
  putPadPrvKey $ xPrvKey k

-- | Parse a binary extended public key.
getXPubKey :: EgvNetwork -> Get XPubKey
getXPubKey (EgvBtcNetwork net) = do
  ver <- getWord32be
  unless (ver == getExtPubKeyPrefix net) $ fail
      "Get: Invalid version for extended public key"
  XPubKey <$> getWord8
          <*> getWord32be
          <*> getWord32be
          <*> get
          <*> (pubKeyPoint <$> get)
getXPubKey (EgvErgNetwork net) = do
  ver <- getWord32be
  unless (ver == getErgExtPubKeyPrefix net) $ fail
      "Get: Invalid version for extended public key"
  XPubKey <$> getWord8
          <*> getWord32be
          <*> getWord32be
          <*> get
          <*> (pubKeyPoint <$> get)

-- | Serialize an extended public key.
putXPubKey :: EgvNetwork -> Putter XPubKey
putXPubKey (EgvBtcNetwork net) k = do
  putWord32be $ getExtPubKeyPrefix net
  putWord8    $ xPubDepth k
  putWord32be $ xPubParent k
  putWord32be $ xPubIndex k
  put         $ xPubChain k
  put         $ wrapPubKey True (xPubKey k)
putXPubKey (EgvErgNetwork net) k = do
  putWord32be $ getErgExtPubKeyPrefix net
  putWord8    $ xPubDepth k
  putWord32be $ xPubParent k
  putWord32be $ xPubIndex k
  put         $ xPubChain k
  put         $ wrapPubKey True (xPubKey k)

-- | Exports an extended private key to the BIP32 key export format ('Base58').
xPrvExport :: EgvNetwork -> XPrvKey -> Base58
xPrvExport n@(EgvBtcNetwork net) = encodeBase58CheckBtc . runPut . putXPrvKey n
xPrvExport n@(EgvErgNetwork net) = encodeBase58CheckErg . runPut . putXPrvKey n

-- | Exports an extended public key to the BIP32 key export format ('Base58').
xPubExport :: EgvNetwork -> XPubKey -> Base58
xPubExport n@(EgvBtcNetwork net) = encodeBase58CheckBtc . runPut . putXPubKey n
xPubExport n@(EgvErgNetwork net) = encodeBase58CheckErg . runPut . putXPubKey n

-- | Decodes a BIP32 encoded extended private key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPrvImport :: EgvNetwork -> Base58 -> Maybe XPrvKey
xPrvImport n@(EgvBtcNetwork net) = eitherToMaybe . runGet (getXPrvKey n) <=< decodeBase58CheckBtc
xPrvImport n@(EgvErgNetwork net) = eitherToMaybe . runGet (getXPrvKey n) <=< decodeBase58CheckErg

-- | Decodes a BIP32 encoded extended public key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPubImport :: EgvNetwork -> Base58 -> Maybe XPubKey
xPubImport n@(EgvBtcNetwork net) = eitherToMaybe . runGet (getXPubKey n) <=< decodeBase58CheckBtc
xPubImport n@(EgvErgNetwork net) = eitherToMaybe . runGet (getXPubKey n) <=< decodeBase58CheckErg

-- | Wrapper for a root extended private key (a key without assigned network)
newtype EgvRootXPrvKey = EgvRootXPrvKey {unEgvRootXPrvKey :: XPrvKey}
  deriving (Eq, Show, Read)

instance ToJSON EgvRootXPrvKey where
  toJSON (EgvRootXPrvKey XPrvKey{..}) = object [
      "depth"  .= toJSON xPrvDepth
    , "parent" .= toJSON xPrvParent
    , "index"  .= toJSON xPrvIndex
    , "chain"  .= show xPrvChain
    , "key"    .= show xPrvKey
    ]

instance FromJSON EgvRootXPrvKey where
  parseJSON = withObject "EgvRootXPrvKey" $ \o -> do
    depth  <- o .: "depth"
    parent <- o .: "parent"
    index  <- o .: "index"
    chain  <- o .: "chain"
    key    <- o .: "key"
    case (readMaybe chain, readMaybe key) of
      (Just chain', Just key') -> pure $ EgvRootXPrvKey $ XPrvKey depth parent index chain' key'
      _ -> fail "failed to read chain code or key"

-- | Wrapper around XPrvKey for easy to/from json manipulations
data EgvXPrvKey = EgvXPrvKey {
  egvXPrvCurrency :: Currency
, egvXPrvKey      :: XPrvKey
} deriving (Eq, Show, Read)

-- | Get JSON 'Value' from 'XPrvKey'.
xPrvToJSON :: EgvNetwork -> XPrvKey -> Value
xPrvToJSON net = String . xPrvExport net

-- | Decode an extended private key from a JSON string
xPrvFromJSON :: EgvNetwork -> Value -> Parser XPrvKey
xPrvFromJSON net =
    withText "xprv" $ \t ->
        case xPrvImport net t of
            Nothing -> fail "could not read xprv"
            Just x  -> return x

instance ToJSON EgvXPrvKey where
  toJSON (EgvXPrvKey currency key) = object [
      "currency" .= toJSON currency
    , "prvKey"   .= xPrvToJSON (getCurrencyNetwork currency) key
    ]

instance FromJSON EgvXPrvKey where
  parseJSON = withObject "EgvXPrvKey" $ \o -> do
    currency <- o .: "currency"
    key <- xPrvFromJSON (getCurrencyNetwork currency) =<< (o .: "prvKey")
    pure $ EgvXPrvKey currency key

instance Ord EgvXPrvKey where
  compare (EgvXPrvKey currency1 key1) (EgvXPrvKey currency2 key2) = case compare currency1 currency2 of
    EQ -> compare (xPrvExport (getCurrencyNetwork currency1) key1) (xPrvExport (getCurrencyNetwork currency2) key2)
    x -> x

-- | Wrapper around XPubKey for easy to/from json manipulations
data EgvXPubKey = EgvXPubKey {
  egvXPubCurrency :: Currency
, egvXPubKey      :: XPubKey
} deriving (Eq, Show, Read)

-- | Get JSON 'Value' from 'XPubKey'.
xPubToJSON :: EgvNetwork -> XPubKey -> Value
xPubToJSON net = String . xPubExport net

-- | Decode an extended public key from a JSON string
xPubFromJSON :: EgvNetwork -> Value -> Parser XPubKey
xPubFromJSON net =
    withText "xpub" $ \t ->
        case xPubImport net t of
            Nothing -> fail "could not read xpub"
            Just x  -> return x

instance ToJSON EgvXPubKey where
  toJSON (EgvXPubKey currency key) = object [
      "currency" .= toJSON currency
    , "pubKey"  .= xPubToJSON (getCurrencyNetwork currency) key
    ]

instance FromJSON EgvXPubKey where
  parseJSON = withObject "EgvXPubKey" $ \o -> do
    currency <- o .: "currency"
    key <- xPubFromJSON (getCurrencyNetwork currency) =<< (o .: "pubKey")
    pure $ EgvXPubKey currency key

instance Ord EgvXPubKey where
  compare (EgvXPubKey currency1 key1) (EgvXPubKey currency2 key2) = case compare currency1 currency2 of
    EQ -> compare (xPubExport (getCurrencyNetwork currency1) key1) (xPubExport (getCurrencyNetwork currency2) key2)
    x -> x

data EgvPrvKeyсhain = EgvPrvKeyсhain {
  egvPrvKeyсhain'master   :: EgvXPrvKey
  -- ^Extended private key with BIP44 derivation path /m\/purpose'\/coin_type'\/account'/.
, egvPrvKeyсhain'external :: MI.IntMap EgvXPrvKey
  -- ^Map with BIP44 external extended private keys and corresponding indices.
  -- This private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/0\/address_index/.
, egvPrvKeyсhain'internal :: MI.IntMap EgvXPrvKey
  -- ^Map with BIP44 internal extended private keys and corresponding indices.
  -- This private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/1\/address_index/.
} deriving (Eq, Show, Read)

$(deriveJSON aesonOptionsStripToApostroph ''EgvPrvKeyсhain)

data EgvPubKeyсhain = EgvPubKeyсhain {
  egvPubKeyсhain'master   :: EgvXPubKey
  -- ^Extended public key with BIP44 derivation path /m\/purpose'\/coin_type'\/account'/.
, egvPubKeyсhain'external :: MI.IntMap EgvXPubKey
  -- ^Map with BIP44 external extended public keys and corresponding indices.
  -- This addresses must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/0\/address_index/.
, egvPubKeyсhain'internal :: MI.IntMap EgvXPubKey
  -- ^Map with BIP44 internal extended public keys and corresponding indices.
  -- This addresses must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/1\/address_index/.
} deriving (Eq, Show, Read)

$(deriveJSON aesonOptionsStripToApostroph ''EgvPubKeyсhain)

-- | Supported key purposes. It represents /change/ field in BIP44 derivation path.
-- External chain is used for addresses that are meant to be visible outside of the wallet (e.g. for receiving payments).
-- Internal chain is used for addresses which are not meant to be visible outside of the wallet and is used for return transaction change.
data KeyPurpose = External | Internal
  deriving (Eq, Ord, Show, Read)
