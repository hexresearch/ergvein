module Ergvein.Types.Keys(
    XPrvKey(..)
  , XPubKey(..)
  , EgvRootXPrvKey(..)
  , EgvXPrvKey(..)
  , EgvXPubKey(..)
  , EgvPrvKeyсhain(..)
  , EgvPubKeyсhain(..)
  , KeyPurpose(..)
  ) where

import Data.Aeson
import Ergvein.Aeson
import Ergvein.Types.Currency
import Network.Haskoin.Keys
import Text.Read(readMaybe)

import qualified Data.IntMap.Strict as MI

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

instance ToJSON EgvXPrvKey where
  toJSON (EgvXPrvKey currency key) = object [
      "currency" .= toJSON currency
    , "prvKey"  .= xPrvToJSON (getCurrencyNetwork currency) key
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
