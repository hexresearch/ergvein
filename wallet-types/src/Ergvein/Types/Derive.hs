module Ergvein.Types.Derive(
    deriveCurrencyMasterPrvKey
  , deriveCurrencyMasterPubKey
  , derivePrvKey
  , derivePubKey
  , DerivPrefix
  , defaultDerivPathPrefix
  , defaultDerivePath
  , legacyDerivPathPrefix
  , legacyDerivPath
  , parseDerivePath
  , showDerivPath
  , extendDerivPath
  ) where

import Data.Maybe
import Data.Text (Text)
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Text.Read (readMaybe)

import qualified Data.ByteString       as BS
import qualified Data.Text             as T

-- | Shorthand for encoded BIP48 derivation path like m/84'/0'/0' (hard)
type DerivPrefix = [KeyIndex]

-- | Derivation path starts with 84 according to BIP84
defaultDerivPathPrefix :: DerivPrefix
defaultDerivPathPrefix = [84]

-- | Old wallets used m/44' prefix
legacyDerivPathPrefix :: DerivPrefix
legacyDerivPathPrefix = [44]

-- | Remember that we used to use 44' prefix in old wallets
legacyDerivPath :: Currency -> DerivPrefix
legacyDerivPath currency = extendDerivPath currency legacyDerivPathPrefix

-- | Derivation path from BIP44 that compatible with BIP84
defaultDerivePath :: Currency -> DerivPrefix
defaultDerivePath currency = extendDerivPath currency defaultDerivPathPrefix

-- | Parse string m/0'/0'/0' as derivation path
parseDerivePath :: Text -> Maybe DerivPrefix
parseDerivePath s = do
  sm <- T.stripPrefix "m/" s
  ss <- traverse (T.stripSuffix "'") $ T.splitOn "/" sm
  traverse (readMaybe . T.unpack) ss

-- | Display derivation path as string m\/84'\/0'\/0'
showDerivPath :: DerivPrefix -> Text
showDerivPath ks = "m/" <> T.intercalate "/" (fmap ((<> "'") . showt) ks)

-- | Extend derivation path with c'\/0' if it contains only from purpose prefix
extendDerivPath :: Currency -> DerivPrefix -> DerivPrefix
extendDerivPath currency [a] = [a, getCurrencyIndex currency, 0]
extendDerivPath _ as = as

-- | Derive a BIP44 and BIP84 compatible private key for a specific currency.
-- Given a parent private key /m/
-- and a currency with code /c/, this function will compute private key with path /m\/84'\/c'\/0'/.
-- The overide derivation path is expected to be full derivation path
deriveCurrencyMasterPrvKey :: Maybe DerivPrefix -> EgvRootXPrvKey -> Currency -> EgvXPrvKey
deriveCurrencyMasterPrvKey mpath rootPrvKey currency =
    let hardPath = fromMaybe (defaultDerivePath currency) mpath
        derivedPrvKey = foldl hardSubKey (unEgvRootXPrvKey rootPrvKey) hardPath
    in case currency of
      BTC -> BtcXPrvKey derivedPrvKey
      ERGO -> ErgXPrvKey derivedPrvKey

-- | Derive a BIP44 compatible public key for a specific currency.
-- Given a parent private key /m/
-- and a currency with code /c/, this function will compute public key with path /m\/84'\/c'\/0'/.
deriveCurrencyMasterPubKey :: Maybe DerivPrefix -> EgvRootXPrvKey -> Currency -> EgvXPubKey
deriveCurrencyMasterPubKey mpath rootPrvKey currency =
    let hardPath = fromMaybe (defaultDerivePath currency) mpath
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
