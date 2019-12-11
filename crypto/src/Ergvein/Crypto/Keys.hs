module Ergvein.Crypto.Keys(
    Base58
  , encodeBase58
  , decodeBase58
  , Mnemonic
  , Seed
  , toMnemonic
  , mnemonicToSeed
  , Currency(..)
  , xPrvImport
  , xPrvExport
  , xPubImport
  , xPubExport
  , EgvXPrvKey(..)
  , EgvXPubKey(..)
  , EgvRootKey(..)
  , getEntropy
  , makeXPrvKey
  , deriveXPubKey
  , xPubAddr
  , addrToString
  , xPubErgAddrString
  , KeyIndex
  , deriveCurrencyMasterKey
  , deriveExternalKey
  , deriveInternalKey
  , example
  ) where

import Crypto.Hash
import Crypto.Hash.Algorithms
import Data.Aeson
import Data.Text hiding (foldl)
import Ergvein.Crypto.Constants
import Ergvein.Crypto.WordLists
import Ergvein.Types.Currency (Currency(..))
import Network.Haskoin.Address
import Network.Haskoin.Address.Base58
import Network.Haskoin.Constants
import Network.Haskoin.Keys
import Network.Haskoin.Util
import Text.Read (readMaybe)

import qualified Data.ByteArray  as BA
import qualified Data.ByteString as BS
import qualified System.Entropy  as E

-- | Wrapper for a root key (a key without assigned network)
newtype EgvRootKey = EgvRootKey {unEgvRootKey :: XPrvKey}
  deriving (Eq, Show, Read)

instance ToJSON EgvRootKey where
  toJSON (EgvRootKey XPrvKey{..}) = object [
      "depth"  .= toJSON xPrvDepth
    , "parent" .= toJSON xPrvParent
    , "index"  .= toJSON xPrvIndex
    , "chain"  .= show xPrvChain
    , "key"    .= show xPrvKey
    ]

instance FromJSON EgvRootKey where
  parseJSON = withObject "EgvRootKey" $ \o -> do
    depth  <- o .: "depth"
    parent <- o .: "parent"
    index  <- o .: "index"
    chain  <- o .: "chain"
    key    <- o .: "key"
    case (readMaybe chain, readMaybe key) of
      (Just chain', Just key') -> pure $ EgvRootKey $ XPrvKey depth parent index chain' key'
      _ -> fail "failed to read chain code or key"

-- | Wrapper around XPrvKey for easy to/from json manipulations
data EgvXPrvKey = EgvXPrvKey {
  egvXPrvCurrency :: Currency
, egvXPrvKey      :: XPrvKey
} deriving (Eq)

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
} deriving (Eq)

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

getEntropy :: IO Entropy
getEntropy = E.getEntropy defaultEntropyLength

-- | Convert BTC extended public key to a human-readable string.
xPubBtcAddrString :: Network -> XPubKey -> Text
xPubBtcAddrString net key = addrToString net addr
  where addr = xPubAddr key

-- | Convert ERGO extended public key to a human-readable string.
xPubErgAddrString :: Network -> XPubKey -> Text
xPubErgAddrString net key = encodeBase58 content
 where
  prefix          = BS.singleton $ getAddrPrefix net
  keyByteString   = exportPubKey True (xPubKey key)
  checkSumContent = BS.append prefix keyByteString
  checksum        = BA.convert $ hashWith Blake2b_256 checkSumContent :: BS.ByteString
  content         = BS.take 38 (BS.concat [prefix, keyByteString, checksum])

-- | Convert extended public key to a human-readable string.
xPubAddrToString :: Network -> XPubKey -> Either String Text
xPubAddrToString net key
  | net == btc || net == btcTest = Right $ xPubBtcAddrString net key
  | net == erg || net == ergTest = Right $ xPubErgAddrString net key
  | otherwise                    = Left "Unknown network type"

-- | Derive a BIP44 compatible key for a specific currency.
-- Given a parent key /m/
-- and a currency with code /c/, this function will compute /m\/44'\/c'\/0/.
deriveCurrencyMasterKey :: EgvRootKey -> Currency -> EgvXPrvKey
deriveCurrencyMasterKey rootKey currency =
    let path = [44, getCurrencyIndex currency, 0]
        derivedKey = foldl hardSubKey (unEgvRootKey rootKey) path
    in EgvXPrvKey currency derivedKey

-- | Derive a BIP44 compatible external key with a given index.
-- Given a parent key /m/ and an index /i/, this function will compute /m\/0\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterKey' as the first argument of this function.
deriveExternalKey :: EgvXPrvKey -> KeyIndex -> EgvXPrvKey
deriveExternalKey masterKey index =
  let path = [0, index]
      mKey = egvXPrvKey masterKey
      currency = egvXPrvCurrency masterKey
      derivedKey = foldl prvSubKey mKey path
  in EgvXPrvKey currency derivedKey

-- | Derive a BIP44 compatible internal key (also known as change addresses) with a given index.
-- Given a parent key /m/ and an index /i/, this function will compute /m\/1\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterKey' as the first argument of this function.
deriveInternalKey :: EgvXPrvKey -> KeyIndex -> EgvXPrvKey
deriveInternalKey masterKey index =
  let path = [1, index]
      mKey = egvXPrvKey masterKey
      currency = egvXPrvCurrency masterKey
      derivedKey = foldl prvSubKey mKey path
  in EgvXPrvKey currency derivedKey

example :: IO ()
example = do
  ent <- getEntropy
  putStrLn "Entropy:"
  print ent
  let mnemonic = toMnemonic ent
  putStrLn "\nMnemonic:"
  print mnemonic
  let seed = mnemonic >>= mnemonicToSeed BS.empty
  putStrLn "\nSeed:"
  print seed
  let xPrvKey = fmap makeXPrvKey seed
  putStrLn "\nExtended private key:"
  print xPrvKey
  let xPubKey = fmap deriveXPubKey xPrvKey
  putStrLn "\nExtended public key:"
  print xPubKey
  let network = erg
  let address = fmap (xPubAddrToString network) xPubKey
  putStrLn "\nAddress:"
  print address
