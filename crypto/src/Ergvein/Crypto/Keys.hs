module Ergvein.Crypto.Keys(
    Base58
  , encodeBase58
  , decodeBase58
  , Mnemonic
  , Seed
  , toMnemonic
  , mnemonicToSeed
  , EgvPrvKeyсhain(..)
  , Currency(..)
  , XPubKey
  , XPrvKey
  , xPubImport
  , xPrvImport
  , xPubExport
  , xPrvExport
  , getEntropy
  , makeXPrvKey
  , deriveXPubKey
  , deriveCurrencyMasterKey
  , xPubAddr
  , addrToString
  , xPubErgAddrString
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

import qualified Data.ByteArray                 as BA
import qualified Data.ByteString                as BS
import qualified Data.IntMap.Strict             as MI
import qualified System.Entropy                 as E

data EgvPrvKeyсhain = EgvPrvKeyсhain {
  egvPrvKeyсhain'base     :: XPrvKey
  -- ^The first part of BIP44 key with derivation path /m\/purpose'\/coin_type'\/account'/.
, egvPrvKeyсhain'external :: MI.IntMap XPrvKey
  -- ^Map with BIP44 external keys.
  -- Private key indices are Map keys, and the private keys are Map values.
  -- Private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/0\/address_index/.
, egvPrvKeyсhain'internal :: MI.IntMap XPrvKey
  -- ^Map with BIP44 internal keys.
  -- Private key indices are Map keys, and the private keys are Map values.
  -- Private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/1\/address_index/.
} deriving (Eq)

-- | Derive a BIP44 compatible key for a specific currency.
-- Given a parent key /m/
-- and a currency with code /c/, this function will compute /m\/44'\/c'\/0/.
deriveCurrencyMasterKey :: XPrvKey -> Currency -> XPrvKey
deriveCurrencyMasterKey pk cur =
  let path = [44, getCurrencyIndex cur, 0]
      key  = foldl hardSubKey pk path
  in key

-- | Derive a BIP44 compatible external key with a given index.
-- Given a parent key /m/ and an index /i/, this function will compute /m\/0\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterKey' as the first argument of this function.
deriveExternalKey :: XPrvKey -> KeyIndex -> XPrvKey
deriveExternalKey pk index =
  let path = [0, index]
      key  = foldl prvSubKey pk path
  in key

-- | Derive a BIP44 compatible internal key (also known as change addresses) with a given index.
-- Given a parent key /m/ and an index /i/, this function will compute /m\/1\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterKey' as the first argument of this function.
deriveInternalKey :: XPrvKey -> KeyIndex -> XPrvKey
deriveInternalKey pk index =
  let path = [1, index]
      key  = foldl prvSubKey pk path
  in key

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

instance ToJSON XPrvKey where
  toJSON XPrvKey{..} = object [
      "d" .= toJSON xPrvDepth
    , "p" .= toJSON xPrvParent
    , "i" .= toJSON xPrvIndex
    , "c" .= show xPrvChain
    , "k" .= show xPrvKey
    ]

instance FromJSON XPrvKey where
  parseJSON = withObject "XPrvKey" $ \o -> do
    d <- o .: "d"
    p <- o .: "p"
    i <- o .: "i"
    c <- o .: "c"
    k <- o .: "k"
    case (readMaybe c, readMaybe k) of
      (Just c', Just k') -> pure $ XPrvKey d p i c' k'
      _ -> fail "failed to read c or k"
