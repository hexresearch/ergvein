module Ergvein.Wallet.Storage.Keys (
    deriveCurrencyMasterPrvKey
  , deriveCurrencyMasterPubKey
  , derivePrvKey
  , derivePubKey
  ) where

import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network

-- | Derive a BIP44 compatible private key for a specific currency.
-- Given a parent private key /m/
-- and a currency with code /c/, this function will compute private key with path /m\/44'\/c'\/0'/.
deriveCurrencyMasterPrvKey :: EgvRootXPrvKey -> Currency -> EgvXPrvKey
deriveCurrencyMasterPrvKey rootPrvKey currency =
    let hardPath = [44, getCurrencyIndex currency, 0]
        derivedPrvKey = foldl hardSubKey (unEgvRootXPrvKey rootPrvKey) hardPath
    in case currency of
      BTC -> BtcXPrvKey derivedPrvKey
      ERGO -> ErgXPrvKey derivedPrvKey

-- | Derive a BIP44 compatible public key for a specific currency.
-- Given a parent private key /m/
-- and a currency with code /c/, this function will compute public key with path /m\/44'\/c'\/0'/.
deriveCurrencyMasterPubKey :: EgvRootXPrvKey -> Currency -> EgvXPubKey
deriveCurrencyMasterPubKey rootPrvKey currency =
    let hardPath = [44, getCurrencyIndex currency, 0]
        derivedPrvKey = foldl hardSubKey (unEgvRootXPrvKey rootPrvKey) hardPath
        derivedPubKey = deriveXPubKey derivedPrvKey
    in case currency of
      ERGO -> ErgXPubKey derivedPubKey ""
      BTC -> BtcXPubKey derivedPubKey ""

-- | Derive a BIP44 compatible private key with a given purpose (external or internal) and index.
-- Given a parent private key /m/, purpose /p/ and an index /i/, this function will compute /m\/p\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterPrvKey' as the first argument of this function.
derivePrvKey :: EgvXPrvKey -> KeyPurpose -> KeyIndex -> EgvXPrvKey
derivePrvKey masterKey keyPurpose index =
  let pCode = if keyPurpose == External then 0 else 1
      path = [pCode, index]
      mKey = unEgvXPrvKey masterKey
      derivedKey = foldl prvSubKey mKey path
  in case masterKey of
    BtcXPrvKey _ -> BtcXPrvKey derivedKey
    ErgXPrvKey _ -> ErgXPrvKey derivedKey

-- | Derive a BIP44 compatible public key with a given purpose (external or internal) and index.
-- Given a parent public key /m/, purpose /p/ and an index /i/, this function will compute /m\/p\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterPubKey' as the first argument of this function.
derivePubKey :: EgvXPubKey -> KeyPurpose -> KeyIndex -> EgvXPubKey
derivePubKey masterKey keyPurpose index =
  let pCode = if keyPurpose == External then 0 else 1
      path = [pCode, index]
      derivedKey mk = foldl pubSubKey mk path
  in case masterKey of
    ErgXPubKey k _ -> ErgXPubKey (derivedKey k) ""
    BtcXPubKey k _ -> BtcXPubKey (derivedKey k) ""
