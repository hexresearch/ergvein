module Ergvein.Crypto.Keys (
    Mnemonic
  , Seed
  , KeyIndex
  , XPrvKey(..)
  , XPubKey(..)
  , SecKeyI(..)
  , PubKeyI(..)
  , toMnemonic
  , mnemonicToSeed
  , makeXPrvKey
  , deriveXPubKey
  , xPubAddr
  , prvSubKey
  , hardSubKey
  , pubKeyWitnessAddr
  , pubSubKey
  , wrapPubKey
  , getPadPrvKey
  , putPadPrvKey
  , bsPadPrvKey
  ) where

import Control.Monad            (unless, when)
import Crypto.Hash              (SHA256 (..), hashWith)
import Crypto.Secp256k1         (SecKey)
import Data.Bits                (shiftR)
import Data.ByteString          (ByteString)
import Data.Serialize.Get       (Get, getWord8)
import Data.Serialize.Put       (Putter, putWord8, runPut)
import Data.Vector              ((!))
import Ergvein.Crypto.WordLists
import Network.Haskoin.Util
import Network.Haskoin.Address
import Network.Haskoin.Keys     hiding (toMnemonic)

import qualified Data.ByteArray  as BA
import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.Vector     as V

-- | Mnemonic key checksum.
type Checksum = ByteString

-- | De-serialize HDW-specific private key.
getPadPrvKey :: Get SecKey
getPadPrvKey = do
    pad <- getWord8
    unless (pad == 0x00) $ fail "Private key must be padded with 0x00"
    secKeyGet

-- | Serialize HDW-specific private key.
putPadPrvKey :: Putter SecKey
putPadPrvKey p = putWord8 0x00 >> secKeyPut p

bsPadPrvKey :: SecKey -> ByteString
bsPadPrvKey = runPut . putPadPrvKey

-- | Compute 'Checksum'.
calcCS :: Int -> Entropy -> Checksum
calcCS len = getBits len . BA.convert . hashWith SHA256

-- | Turn a 'ByteString' into a list of 11-bit numbers.
bsToIndices :: ByteString -> [Int]
bsToIndices bs =
    reverse . go q $ bsToInteger bs `shiftR` r
  where
    (q, r) = (BS.length bs * 8) `quotRem` 11
    go 0 _ = []
    go n i = fromIntegral (i `mod` (fromIntegral $ V.length wordListEnglish)) : go (n - 1) (i `shiftR` 11)

-- | Provide intial 'Entropy' as a 'ByteString' of length multiple of 4 bytes.
-- Output a 'Mnemonic' sentence.
toMnemonic :: Entropy -> Either String Mnemonic
toMnemonic ent = do
    when (BS.null ent) $
        Left "toMnemonic: entropy can not be empty"
    when (remainder /= 0) $
        Left "toMnemonic: entropy must be multiples of 4 bytes"
    when (cs_len > 16) $
        Left "toMnemonic: maximum entropy is 64 bytes (512 bits)"
    return ms
  where
    (cs_len, remainder) = BS.length ent `quotRem` 4
    c = calcCS cs_len ent
    indices = bsToIndices $ ent `BS.append` c
    ms = T.unwords $ map (wordListEnglish!) indices
