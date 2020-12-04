module Ergvein.Types.Keys (
    XPrvKey(..)
  , XPubKey(..)
  , EgvRootXPrvKey(..)
  , EgvRootXPubKey(..)
  , EgvXPrvKey(..)
  , EgvXPubKey(..)
  , EgvPubKeyBox(..)
  , PrvKeystore(..)
  , PubKeystore(..)
  , KeyPurpose(..)
  , ScanKeyBox(..)
  , xPrvExport
  , xPrvImport
  , xPubExport
  , xPubImport
  , getLastUnusedKey
  , getPublicKeys
  , egvXPubCurrency
  , getExternalPubKeyIndex
  , extractXPubKeyFromEgv
  , getLabelFromEgvPubKey
  , unEgvXPrvKey
  , egvXPubKeyToEgvAddress
  , xPubToBtcAddr
  , xPubToErgAddr
  , extractAddrs
  ) where

import Control.Monad
import Data.Aeson
import Data.SafeCopy
import Data.Serialize         (get, put)
import Data.Serialize         (Serialize)
import Data.Serialize.Get     (Get, getWord32be, getWord8, runGet)
import Data.Serialize.Put     (Putter, putWord32be, putWord8, runPut)
import Data.Text              (Text)
import Data.Vector            (Vector)
import Ergvein.Aeson
import Ergvein.Crypto.Keys
import Ergvein.Crypto.Util
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Network
import Ergvein.Types.Orphanage()
import Ergvein.Types.Transaction
import GHC.Generics

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize        as SE

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
xPrvExport n@(EgvBtcNetwork _) = encodeBase58CheckBtc . runPut . putXPrvKey n
xPrvExport n@(EgvErgNetwork _) = encodeBase58CheckErg . runPut . putXPrvKey n

-- | Exports an extended public key to the BIP32 key export format ('Base58').
xPubExport :: EgvNetwork -> XPubKey -> Base58
xPubExport n@(EgvBtcNetwork _) = encodeBase58CheckBtc . runPut . putXPubKey n
xPubExport n@(EgvErgNetwork _) = encodeBase58CheckErg . runPut . putXPubKey n

-- | Decodes a BIP32 encoded extended private key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPrvImport :: EgvNetwork -> Base58 -> Maybe XPrvKey
xPrvImport n@(EgvBtcNetwork _) = eitherToMaybe . runGet (getXPrvKey n) <=< decodeBase58CheckBtc
xPrvImport n@(EgvErgNetwork _) = eitherToMaybe . runGet (getXPrvKey n) <=< decodeBase58CheckErg

-- | Decodes a BIP32 encoded extended public key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPubImport :: EgvNetwork -> Base58 -> Maybe XPubKey
xPubImport n@(EgvBtcNetwork _) = eitherToMaybe . runGet (getXPubKey n) <=< decodeBase58CheckBtc
xPubImport n@(EgvErgNetwork _) = eitherToMaybe . runGet (getXPubKey n) <=< decodeBase58CheckErg

-- | Wrapper for a root extended private key (a key without assigned network)
newtype EgvRootXPrvKey = EgvRootXPrvKey {unEgvRootXPrvKey :: XPrvKey}
  deriving (Eq, Show, Read)

instance Serialize EgvRootXPrvKey where
  get = fmap EgvRootXPrvKey $ XPrvKey
    <$> getWord8
    <*> getWord32be
    <*> getWord32be
    <*> get
    <*> getPadPrvKey

  put (EgvRootXPrvKey k) = do
    putWord8     $ xPrvDepth k
    putWord32be  $ xPrvParent k
    putWord32be  $ xPrvIndex k
    put          $ xPrvChain k
    putPadPrvKey $ xPrvKey k

-- | Wrapper for a root extended public key (a key without assigned network)
newtype EgvRootXPubKey = EgvRootXPubKey {unEgvRootXPubKey :: XPubKey}
  deriving (Eq, Show, Read)

instance Serialize EgvRootXPubKey where
  get = fmap EgvRootXPubKey $ XPubKey
    <$> getWord8
    <*> getWord32be
    <*> getWord32be
    <*> get
    <*> (pubKeyPoint <$> get)
  put (EgvRootXPubKey k) = do
    putWord8    $ xPubDepth k
    putWord32be $ xPubParent k
    putWord32be $ xPubIndex k
    put         $ xPubChain k
    put         $ wrapPubKey True (xPubKey k)

-- | Wrapper around XPrvKey for easy to/from json manipulations
data EgvXPrvKey = BtcXPrvKey { btcXPrvKey :: !XPrvKey} | ErgXPrvKey {ergXPrvKey :: !XPrvKey}
  deriving (Eq, Show, Read)

egvXPrvKeyGetter :: Get EgvXPrvKey
egvXPrvKeyGetter = do
  cur <- get
  k <- getXPrvKey (getCurrencyNetwork cur)
  pure $ case cur of
    BTC -> BtcXPrvKey k
    ERGO -> ErgXPrvKey k

egvXPrvKeyPutter :: Putter EgvXPrvKey
egvXPrvKeyPutter key = case key of
  ErgXPrvKey k -> do
    put ERGO
    putXPrvKey (getCurrencyNetwork ERGO) k
  BtcXPrvKey k -> do
    put BTC
    putXPrvKey (getCurrencyNetwork BTC) k

instance Serialize EgvXPrvKey where
  get = egvXPrvKeyGetter
  put = egvXPrvKeyPutter

unEgvXPrvKey :: EgvXPrvKey -> XPrvKey
unEgvXPrvKey key = case key of
  BtcXPrvKey k -> k
  ErgXPrvKey k -> k

-- | Wrapper around XPubKey for easy to/from json manipulations
data EgvXPubKey =
    ErgXPubKey {
      ergXPubKey   :: XPubKey
    , ergXPubLabel :: Text
    }
  | BtcXPubKey {
      btcXPubKey   :: XPubKey
    , btcXPubLabel :: Text
    }
  deriving (Eq, Show, Read)

egvXPubKeyGetter :: Get EgvXPubKey
egvXPubKeyGetter = do
  cur <- get
  k <- getXPubKey (getCurrencyNetwork cur)
  l <- fmap T.pack get
  pure $ case cur of
    BTC -> BtcXPubKey k l
    ERGO -> ErgXPubKey k l

egvXPubKeyPutter :: Putter EgvXPubKey
egvXPubKeyPutter key = case key of
  ErgXPubKey k l -> do
    put ERGO
    putXPubKey (getCurrencyNetwork ERGO) k
    put $ T.unpack l
  BtcXPubKey k l -> do
    put BTC
    putXPubKey (getCurrencyNetwork BTC) k
    put $ T.unpack l

instance Serialize EgvXPubKey where
  get = egvXPubKeyGetter
  put = egvXPubKeyPutter

egvXPubCurrency :: EgvXPubKey -> Currency
egvXPubCurrency val = case val of
  ErgXPubKey{} -> ERGO
  BtcXPubKey{} -> BTC

instance Ord EgvXPubKey where
  compare key1 key2 = case compare c1 c2 of
    EQ -> compare (xPubExport (getCurrencyNetwork c1) k1) (xPubExport (getCurrencyNetwork c2) k2)
    x -> x
    where
      (c1, k1, _l1) =  case key1 of
        ErgXPubKey k l -> (ERGO, k, l)
        BtcXPubKey k l -> (BTC, k, l)
      (c2, k2, _l2) =  case key2 of
        ErgXPubKey k l -> (ERGO, k, l)
        BtcXPubKey k l -> (BTC, k, l)

data PrvKeystore = PrvKeystore {
  prvKeystore'master   :: !EgvXPrvKey
  -- ^Extended private key with BIP44 derivation path /m\/purpose'\/coin_type'\/account'/.
, prvKeystore'external :: !(Vector EgvXPrvKey)
  -- ^Map with BIP44 external extended private keys and corresponding indices.
  -- This private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/0\/address_index/.
, prvKeystore'internal :: !(Vector EgvXPrvKey)
  -- ^Map with BIP44 internal extended private keys and corresponding indices.
  -- This private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/1\/address_index/.
} deriving (Eq, Show, Read, Generic, Serialize)


instance SafeCopy PrvKeystore where
  putCopy (PrvKeystore m e i) = contain $
    put m >> put e >> put i
  getCopy = contain $
    PrvKeystore <$> get <*> get <*> get

data EgvPubKeyBox = EgvPubKeyBox {
  pubKeyBox'key    :: !EgvXPubKey
, pubKeyBox'txs    :: !(S.Set TxId)
, pubKeyBox'manual :: !Bool
} deriving (Eq, Show, Read, Generic, Serialize)

instance SafeCopy EgvPubKeyBox where
  putCopy (EgvPubKeyBox k t m) = contain $
    put k >> put t >> put m
  getCopy = contain $
    EgvPubKeyBox <$> get <*> get <*> get

data PubKeystore = PubKeystore {
  pubKeystore'master   :: !EgvXPubKey
  -- ^Extended public key with BIP44 derivation path /m\/purpose'\/coin_type'\/account'/.
, pubKeystore'external :: !(Vector EgvPubKeyBox)
  -- ^Map with BIP44 external extended public keys and corresponding indices.
  -- This addresses must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/0\/address_index/.
, pubKeystore'internal :: !(Vector EgvPubKeyBox)
  -- ^Map with BIP44 internal extended public keys and corresponding indices.
  -- This addresses must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/1\/address_index/.
} deriving (Eq, Show, Read, Generic, Serialize)

instance SafeCopy PubKeystore where
  putCopy (PubKeystore m e i) = contain $
    put m >> safePut e >> safePut i
  getCopy = contain $
    PubKeystore <$> get <*> safeGet <*> safeGet

getLastUnusedKey :: KeyPurpose -> PubKeystore -> Maybe (Int, EgvPubKeyBox)
getLastUnusedKey kp PubKeystore{..} = go Nothing vector
  where
    vector = case kp of
      Internal -> pubKeystore'internal
      External -> pubKeystore'external
    go :: Maybe (Int, EgvPubKeyBox) -> Vector EgvPubKeyBox -> Maybe (Int, EgvPubKeyBox)
    go mk vec = if V.null vec then mk else let
      kb@(EgvPubKeyBox _ txs m) = V.last vec
      in if m || not (S.null txs)
        then mk
        else go (Just (V.length vec - 1, kb)) $ V.init vec

data ScanKeyBox = ScanKeyBox {
  scanBox'key     :: !EgvXPubKey
, scanBox'purpose :: !KeyPurpose
, scanBox'index   :: !Int
} deriving (Show)

-- | Get all public keys in storage (external and internal) to scan for new transactions for them.
getPublicKeys :: PubKeystore -> Vector ScanKeyBox
getPublicKeys PubKeystore{..} = ext <> int
  where
    ext = V.imap (\i kb -> ScanKeyBox (pubKeyBox'key kb) External i) pubKeystore'external
    int = V.imap (\i kb -> ScanKeyBox (pubKeyBox'key kb) Internal i) pubKeystore'internal

getExternalPubKeyIndex :: PubKeystore -> Int
getExternalPubKeyIndex = V.length . pubKeystore'external

extractXPubKeyFromEgv :: EgvXPubKey -> XPubKey
extractXPubKeyFromEgv key = case key of
  ErgXPubKey k _ -> k
  BtcXPubKey k _ -> k

getLabelFromEgvPubKey :: EgvXPubKey -> Text
getLabelFromEgvPubKey key = case key of
  ErgXPubKey _ l -> l
  BtcXPubKey _ l -> l


xPubToBtcAddr :: XPubKey -> BtcAddress
xPubToBtcAddr key = pubKeyWitnessAddr $ wrapPubKey True (xPubKey key)

xPubToErgAddr :: XPubKey -> ErgAddress
xPubToErgAddr key = pubKeyErgAddr $ wrapPubKey True (xPubKey key)

pubKeyErgAddr :: PubKeyI -> ErgAddress
pubKeyErgAddr = ErgPubKeyAddress . VLAddr . BSS.toShort . SE.encode

egvXPubKeyToEgvAddress :: EgvXPubKey -> EgvAddress
egvXPubKeyToEgvAddress key = case key of
  ErgXPubKey k _ -> ErgAddress $ xPubToErgAddr k
  BtcXPubKey k _ -> BtcAddress $ xPubToBtcAddr k

-- | Extract addresses from keystore
extractAddrs :: PubKeystore -> Vector EgvAddress
extractAddrs pks = fmap (egvXPubKeyToEgvAddress . scanBox'key) $ getPublicKeys pks

-- | Supported key purposes. It represents /change/ field in BIP44 derivation path.
-- External chain is used for addresses that are meant to be visible outside of the wallet (e.g. for receiving payments).
-- Internal chain is used for addresses which are not meant to be visible outside of the wallet and is used for return transaction change.
data KeyPurpose = External | Internal
  deriving (Eq, Ord, Show, Read, Generic, Serialize)
$(deriveJSON defaultOptions ''KeyPurpose)
