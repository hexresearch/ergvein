-- Primary stuff, that is required for all versions of Keys package
module Ergvein.Types.Keys.Prim
  (
    KeyPurpose(..)
  , EgvRootXPrvKey(..)
  , EgvXPrvKey(..)
  , EgvRootXPubKey(..)
  , EgvXPubKey(..)
  , getXPrvKey
  , putXPrvKey
  , getXPubKey
  , putXPubKey
  , xPrvExport
  , xPubExport
  , xPrvImport
  , xPubImport
  ) where

import Control.Monad
import Data.Serialize         (Serialize, get, put)
import Data.Serialize.Get     (Get, getWord32be, getWord8, runGet)
import Data.Serialize.Put     (Putter, putWord32be, putWord8, runPut)
import Data.Text              (Text, pack, unpack)
import GHC.Generics

import Ergvein.Aeson
import Ergvein.Crypto.Keys
import Ergvein.Crypto.Util
import Ergvein.Types.Currency
import Ergvein.Types.Network
import Ergvein.Types.Orphanage()

-- | Supported key purposes. It represents /change/ field in BIP44 derivation path.
-- External chain is used for addresses that are meant to be visible outside of the wallet (e.g. for receiving payments).
-- Internal chain is used for addresses which are not meant to be visible outside of the wallet and is used for return transaction change.
data KeyPurpose = External | Internal
  deriving (Eq, Ord, Show, Read, Generic, Serialize)
$(deriveJSON defaultOptions ''KeyPurpose)

-- ====================================================================
--      Private keys: EgvRootXPrvKey, EgvXPrvKey
-- ====================================================================

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

-- | Wrapper around XPrvKey for easy to/from json manipulations
data EgvXPrvKey = BtcXPrvKey { btcXPrvKey :: !XPrvKey} | ErgXPrvKey {ergXPrvKey :: !XPrvKey}
  deriving (Eq, Show, Read)

instance Serialize EgvXPrvKey where
  get = do
    cur <- get
    k <- getXPrvKey (getCurrencyNetwork cur)
    pure $ case cur of
      BTC -> BtcXPrvKey k
      ERGO -> ErgXPrvKey k
  put key = case key of
    ErgXPrvKey k -> do
      put ERGO
      putXPrvKey (getCurrencyNetwork ERGO) k
    BtcXPrvKey k -> do
      put BTC
      putXPrvKey (getCurrencyNetwork BTC) k

-- ====================================================================
--      Public keys: EgvRootXPubKey, EgvXPubKey
-- ====================================================================

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

-- | Wrapper around XPubKey for easy to/from json manipulations
data EgvXPubKey =
    ErgXPubKey {
      ergXPubKey   :: !XPubKey
    , ergXPubLabel :: !Text
    }
  | BtcXPubKey {
      btcXPubKey   :: !XPubKey
    , btcXPubLabel :: !Text
    }
  deriving (Eq, Show, Read)

instance Serialize EgvXPubKey where
  get = do
    cur <- get
    k <- getXPubKey (getCurrencyNetwork cur)
    l <- fmap pack get
    pure $ case cur of
      BTC -> BtcXPubKey k l
      ERGO -> ErgXPubKey k l
  put key = case key of
    ErgXPubKey k l -> do
      put ERGO
      putXPubKey (getCurrencyNetwork ERGO) k
      put $ unpack l
    BtcXPubKey k l -> do
      put BTC
      putXPubKey (getCurrencyNetwork BTC) k
      put $ unpack l

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

-- ====================================================================
--      Getters and putters for base crypto keys
-- ====================================================================

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
