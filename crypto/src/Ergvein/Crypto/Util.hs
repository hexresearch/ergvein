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
  , checkSum32Erg
  , encodeBase58CheckBtc
  , encodeBase58CheckErg
  , decodeBase58CheckBtc
  , decodeBase58CheckErg
) where

import Control.Monad (guard)
import Crypto.Hash

import Data.ByteArray (ByteArrayAccess)
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Serialize (encode, decode)
import Network.Haskoin.Address.Base58
import Network.Haskoin.Crypto (CheckSum32, checkSum32)
import Network.Haskoin.Keys (Entropy)
import Network.Haskoin.Util (encodeHex, decodeHex)

import qualified Data.ByteArray        as BA
import qualified Data.ByteString       as BS
import qualified System.Entropy        as E

-- | According to the BIP32 the allowed size of entropy is between 16 and 64 bytes (32 bytes is advised).
-- The mnemonic must encode entropy in a multiple of 4 bytes.
-- With 32 bytes of entropy generated mnemonic will contain 24 words.
defaultEntropyLength :: Int
defaultEntropyLength = 32

getEntropy :: IO Entropy
getEntropy = E.getEntropy defaultEntropyLength

-- | Computes a 32 bit checksum for ERGO cryptocurrency.
checkSum32Btc :: ByteArrayAccess b => b -> CheckSum32
checkSum32Btc = checkSum32

-- | Computes a 32 bit checksum for ERGO cryptocurrency.
checkSum32Erg :: ByteArrayAccess b => b -> CheckSum32
checkSum32Erg = fromRight (error "Colud not decode bytes as CheckSum32")
             . decode
             . BS.take 4
             . BA.convert
             . hashWith Blake2b_256

-- | Computes a checksum for the input 'ByteString' and encodes the input and
-- the checksum as 'Base58' for BTC.
encodeBase58CheckBtc :: ByteString -> Base58
encodeBase58CheckBtc = encodeBase58Check

-- | Computes a checksum for the input 'ByteString' and encodes the input and
-- the checksum as 'Base58' for ERGO.
encodeBase58CheckErg :: ByteString -> Base58
encodeBase58CheckErg bs =
  encodeBase58 $ BS.append bs $ encode $ checkSum32Erg bs

-- | Decode a 'Base58'-encoded string that contains a checksum for BTC. This function
-- returns 'Nothing' if the input string contains invalid 'Base58' characters or
-- if the checksum fails.
decodeBase58CheckBtc :: Base58 -> Maybe ByteString
decodeBase58CheckBtc = decodeBase58Check

-- | Decode a 'Base58'-encoded string that contains a checksum for ERGO. This function
-- returns 'Nothing' if the input string contains invalid 'Base58' characters or
-- if the checksum fails.
decodeBase58CheckErg :: Base58 -> Maybe ByteString
decodeBase58CheckErg bs = do
  rs <- decodeBase58 bs
  let (res, chk) = BS.splitAt (BS.length rs - 4) rs
  guard $ chk == encode (checkSum32Erg res)
  return res
