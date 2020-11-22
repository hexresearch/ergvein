{-# LANGUAGE CPP #-}

module Ergvein.Types.Network (
    BtcNetwork
  , Network(..)
  , ErgNetwork(..)
  , EgvNetwork(..)
  , btc
  , btcTest
  , erg
  , ergTest
  , getCurrencyNetwork
  , getCurrencyIndex
  ) where

import Data.Flat
import Data.Word
import Ergvein.Crypto.Keys
import Ergvein.Types.Currency
import Network.Haskoin.Constants

type BtcNetwork = Network

data ErgNetwork = ErgNetwork
  { -- | lowercase alphanumeric and dashes
    getErgNetworkName              :: !String
    -- | network Haskell identifier
  , getErgNetworkIdent             :: !String
    -- | prefix for 'Base58' P2PK addresses
  , getErgAddrPrefix               :: !Word8
 -- | prefix for 'Base58' P2SH addresses
  , getErgScriptHashPrefix         :: !Word8
    -- | prefix for 'Base58' P2S addresses
  , getErgScriptPrefix             :: !Word8
    -- | prefix for WIF private key
  , getErgSecretPrefix             :: !Word8
    -- | prefix for extended public key
  , getErgExtPubKeyPrefix          :: !Word32
    -- | prefix for extended private key
  , getErgExtSecretPrefix          :: !Word32
    -- | BIP44 derivation path root
  , getErgBip44Coin                :: !Word32
  } deriving (Eq, Generic)

data EgvNetwork
  = EgvBtcNetwork {getBtcNetwork :: BtcNetwork}
  | EgvErgNetwork {getErgNetwork :: ErgNetwork}
  deriving (Eq, Generic)

-- | Ergo network. Symbol: ERG.
erg :: ErgNetwork
erg =
    ErgNetwork
    { getErgNetworkName = "erg"
    , getErgNetworkIdent = "erg"
    , getErgAddrPrefix = 1
    , getErgScriptHashPrefix = 2
    , getErgScriptPrefix = 3
    , getErgSecretPrefix = 128
    , getErgExtPubKeyPrefix = 0x0488b21e
    , getErgExtSecretPrefix = 0x0488ade4
    , getErgBip44Coin = 429
    }

-- | Testnet for Ergo network.
ergTest :: ErgNetwork
ergTest =
    ErgNetwork
    { getErgNetworkName = "ergtest"
    , getErgNetworkIdent = "ergTest"
    , getErgAddrPrefix = 17
    , getErgScriptHashPrefix = 18
    , getErgScriptPrefix = 19
    , getErgSecretPrefix = 239
    , getErgExtPubKeyPrefix = 0x043587cf
    , getErgExtSecretPrefix = 0x04358394
    , getErgBip44Coin = 1
    }

#ifdef TESTNET
-- | Get network correspondent to a given tag
getCurrencyNetwork :: Currency -> EgvNetwork
getCurrencyNetwork t = case t of
  BTC -> EgvBtcNetwork btcTest
  ERGO -> EgvErgNetwork ergTest

getCurrencyIndex :: Currency -> KeyIndex
getCurrencyIndex = const 1
{-# INLINE getCurrencyIndex #-}

#else
getCurrencyNetwork :: Currency -> EgvNetwork
getCurrencyNetwork t = case t of
  BTC -> EgvBtcNetwork btc
  ERGO -> EgvErgNetwork erg

getCurrencyIndex :: Currency -> KeyIndex
getCurrencyIndex t = case t of
  BTC -> getBip44Coin btc
  ERGO -> getErgBip44Coin erg
#endif
