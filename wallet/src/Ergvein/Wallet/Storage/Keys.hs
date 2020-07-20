module Ergvein.Wallet.Storage.Keys (
    egvXPubKeyToEgvAddress
  , deriveCurrencyMasterPrvKey
  , deriveCurrencyMasterPubKey
  , derivePrvKey
  , derivePubKey
  , xPubToBtcAddr
  , xPubToErgAddr
  , extractAddrs
  ) where

import Ergvein.Crypto
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize        as S
import qualified Data.Vector           as V

xPubToBtcAddr :: XPubKey -> BtcAddress
xPubToBtcAddr key = pubKeyWitnessAddr $ wrapPubKey True (xPubKey key)

pubKeyErgAddr :: PubKeyI -> ErgAddress
pubKeyErgAddr = ErgPubKeyAddress . VLAddr . BSS.toShort . S.encode

xPubToErgAddr :: XPubKey -> ErgAddress
xPubToErgAddr key = pubKeyErgAddr $ wrapPubKey True (xPubKey key)

egvXPubKeyToEgvAddress :: EgvXPubKey -> EgvAddress
egvXPubKeyToEgvAddress key = case key of
  ErgXPubKey k _ -> ErgAddress $ xPubToErgAddr k
  BtcXPubKey k _ -> BtcAddress $ xPubToBtcAddr k

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

-- | Extract addresses from keystore
extractAddrs :: PubKeystore -> [(Maybe Int, EgvAddress)]
extractAddrs (PubKeystore mast ext int) = mastadr:(extadrs <> intadrs)
  where
    mastadr = (Nothing,) $ egvXPubKeyToEgvAddress mast
    extadrs = V.toList $ V.imap (\i b -> (Just i, egvXPubKeyToEgvAddress $ pubKeyBox'key b)) ext
    intadrs = V.toList $ V.imap (\i b -> (Nothing, egvXPubKeyToEgvAddress $ pubKeyBox'key b)) int

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
  let address = fmap (egvXPubKeyToEgvAddress . flip BtcXPubKey "") xPubKey
  putStrLn "\nAddress:"
  print address
