module Ergvein.Crypto.Util (
    getEntropy
  , encodeHex
  , decodeHex
  , Base58
  , encodeBase58
  , decodeBase58
  , hashWith
  , Blake2b_256(..)
  , checkSum32Btc
  , encodeBase58CheckBtc
  , decodeBase58CheckBtc
) where

import Crypto.Hash

import Data.ByteArray (ByteArrayAccess)
import Data.ByteString (ByteString)
import Network.Haskoin.Address.Base58
import Network.Haskoin.Crypto (CheckSum32, checkSum32)
import Network.Haskoin.Keys (Entropy)
import Network.Haskoin.Util (encodeHex, decodeHex)

import qualified System.Entropy        as E

-- | According to the BIP32 the allowed size of entropy is between 16 and 64 bytes (32 bytes is advised).
-- The mnemonic must encode entropy in a multiple of 4 bytes.
-- With 16 bytes of entropy generated mnemonic will contain 12 words.
defaultEntropyLength :: Int
defaultEntropyLength = 16

getEntropy :: IO Entropy
getEntropy = E.getEntropy defaultEntropyLength

-- | Computes a 32 bit checksum for Bitcoin.
checkSum32Btc :: ByteArrayAccess b => b -> CheckSum32
checkSum32Btc = checkSum32

-- | Computes a checksum for the input 'ByteString' and encodes the input and
-- the checksum as 'Base58' for BTC.
encodeBase58CheckBtc :: ByteString -> Base58
encodeBase58CheckBtc = encodeBase58Check

-- | Decode a 'Base58'-encoded string that contains a checksum for BTC. This function
-- returns 'Nothing' if the input string contains invalid 'Base58' characters or
-- if the checksum fails.
decodeBase58CheckBtc :: Base58 -> Maybe ByteString
decodeBase58CheckBtc = decodeBase58Check
