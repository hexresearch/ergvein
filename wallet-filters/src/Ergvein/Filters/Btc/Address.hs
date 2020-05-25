-- | Implements BIP-158 like filter for Bech32 addresses. Note
-- that IT IS NOT exact BIP-158 as we don't put all public and redeem scripts
-- inside the filter to save bandwidth.
{-# LANGUAGE BangPatterns #-}
module Ergvein.Filters.Btc.Address
  ( -- * SegWit address
    SegWitAddress(..)
  , guardSegWit
  , fromSegWit
  , getSegWitAddr
  , encodeSegWitAddress
  )
where

import           Data.ByteArray.Hash            ( SipKey(..) )
import           Data.ByteString                ( ByteString )
import           Data.Map.Strict                ( Map )
import           Data.Maybe
import           Data.Serialize                 ( encode )
import           Data.Word
import           Ergvein.Filters.GCS.Mutable
import           Ergvein.Types.Address          (btcAddrToString')
import           GHC.Generics
import           Network.Haskoin.Address
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto         ( Hash160
                                                , Hash256
                                                )
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction

import qualified Data.Attoparsec.Binary        as A
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Binary                   as B
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Map.Strict               as M
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V

-- | Special wrapper around SegWit address (P2WPKH or P2WSH) to distinct it from other types of addresses.
data SegWitAddress = SegWitPubkey !Hash160 | SegWitScript !Hash256
  deriving (Eq, Show, Generic)

-- | Nothing if the given address is not SegWit.
guardSegWit :: Address -> Maybe SegWitAddress
guardSegWit a = case a of
  WitnessPubKeyAddress v -> Just $ SegWitPubkey v
  WitnessScriptAddress v -> Just $ SegWitScript v
  _                      -> Nothing

-- | Unwrap segwit to generic BTC address.
fromSegWit :: SegWitAddress -> Address
fromSegWit a = case a of
  SegWitPubkey v -> WitnessPubKeyAddress v
  SegWitScript v -> WitnessScriptAddress v

-- | Convert address to format that is passed to filter. Address is converted
-- to string and encoded as bytes. Network argument controls whether we are
-- in testnet or mainnet.
encodeSegWitAddress :: Network -> SegWitAddress -> ByteString
encodeSegWitAddress n = T.encodeUtf8 . btcAddrToString' n . fromSegWit

-- | Extract segwit address from transaction output
getSegWitAddr :: TxOut -> Maybe SegWitAddress
getSegWitAddr tout = case decodeOutputBS $ scriptOutput tout of
  Right (PayWitnessPKHash     h) -> Just $ SegWitPubkey h
  Right (PayWitnessScriptHash h) -> Just $ SegWitScript h
  _                              -> Nothing
