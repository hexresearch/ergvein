module Ergvein.Wallet.Storage.Keys(
    egvXPubKeyToEgvAddress
  , deriveCurrencyMasterPrvKey
  , deriveCurrencyMasterPubKey
  , derivePrvKey
  , derivePubKey
  ) where

-- import Crypto.Hash
import Ergvein.Crypto
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Network.Haskoin.Address
import Network.Haskoin.Constants
import Network.Haskoin.Keys

import qualified Data.ByteArray  as BA
import qualified Data.ByteString as BS

-- | Convert BTC extended public key to EgvAddress.
xPubBtcToEgvAddr :: Network -> XPubKey -> EgvAddress
xPubBtcToEgvAddr net key = EgvAddress BTC (BtcAddress address)
  where address = pubKeyWitnessAddr pubKey
        pubKey = PubKeyI (xPubKey key) False

-- | Convert ERGO extended public key to EgvAddress.
xPubErgToEgvAddr :: Network -> XPubKey -> EgvAddress
xPubErgToEgvAddr net key = EgvAddress ERGO (ErgAddress address)
  where prefix          = BS.singleton $ getAddrPrefix net
        keyByteString   = exportPubKey True (xPubKey key)
        checkSumContent = BS.append prefix keyByteString
        checksum        = BA.convert $ hashWith Blake2b_256 checkSumContent :: BS.ByteString
        address         = BS.take 38 (BS.concat [prefix, keyByteString, checksum])

egvXPubKeyToEgvAddress :: EgvXPubKey -> EgvAddress
egvXPubKeyToEgvAddress key
  | currency == BTC = xPubBtcToEgvAddr net xpk
  | currency == ERGO = xPubErgToEgvAddr net xpk
  | otherwise = undefined
  where currency = egvXPubCurrency key
        xpk = egvXPubKey key
        net = getCurrencyNetwork currency

-- | Derive a BIP44 compatible private key for a specific currency.
-- Given a parent private key /m/
-- and a currency with code /c/, this function will compute /m\/44'\/c'\/0/.
deriveCurrencyMasterPrvKey :: EgvRootXPrvKey -> Currency -> EgvXPrvKey
deriveCurrencyMasterPrvKey rootKey currency =
    let hardPath = [44, getCurrencyIndex currency]
        derivedKey = prvSubKey (foldl hardSubKey (unEgvRootXPrvKey rootKey) hardPath) 0
    in EgvXPrvKey currency derivedKey

-- | Derive a BIP44 compatible public key for a specific currency.
-- Given a parent public key /m/
-- and a currency with code /c/, this function will compute /m\/44'\/c'\/0/.
deriveCurrencyMasterPubKey :: EgvRootXPrvKey -> Currency -> EgvXPubKey
deriveCurrencyMasterPubKey rootKey currency =
    let path = [44, getCurrencyIndex currency]
        derivedKey = deriveXPubKey $ prvSubKey (foldl hardSubKey (unEgvRootXPrvKey rootKey) path) 0
    in EgvXPubKey currency derivedKey

-- | Derive a BIP44 compatible private key with a given purpose (external or internal) and index.
-- Given a parent private key /m/, purpose /p/ and an index /i/, this function will compute /m\/p\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterPrvKey' as the first argument of this function.
derivePrvKey :: EgvXPrvKey -> KeyPurpose -> KeyIndex -> EgvXPrvKey
derivePrvKey masterKey keyPurpose index =
  let pCode = if keyPurpose == External then 0 else 1
      path = [pCode, index]
      mKey = egvXPrvKey masterKey
      currency = egvXPrvCurrency masterKey
      derivedKey = foldl prvSubKey mKey path
  in EgvXPrvKey currency derivedKey

-- | Derive a BIP44 compatible public key with a given purpose (external or internal) and index.
-- Given a parent public key /m/, purpose /p/ and an index /i/, this function will compute /m\/p\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterPubKey' as the first argument of this function.
derivePubKey :: EgvXPubKey -> KeyPurpose -> KeyIndex -> EgvXPubKey
derivePubKey masterKey keyPurpose index =
  let pCode = if keyPurpose == External then 0 else 1
      path = [pCode, index]
      mKey = egvXPubKey masterKey
      currency = egvXPubCurrency masterKey
      derivedKey = foldl pubSubKey mKey path
  in EgvXPubKey currency derivedKey

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
  let currency = BTC
  let address = fmap (egvXPubKeyToEgvAddress . EgvXPubKey currency) xPubKey
  putStrLn "\nAddress:"
  print address
