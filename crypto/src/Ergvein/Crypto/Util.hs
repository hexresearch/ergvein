module Ergvein.Crypto.Util (
    getEntropy
  , Base58
  , encodeBase58
  , decodeBase58
  , hashWith
  , Blake2b_256(..)
) where

import Crypto.Hash
import Network.Haskoin.Address.Base58
import Network.Haskoin.Keys

import qualified System.Entropy as E

-- | According to the BIP32 the allowed size of entropy is between 16 and 64 bytes (32 bytes is advised).
-- The mnemonic must encode entropy in a multiple of 4 bytes.
-- With 32 bytes of entropy generated mnemonic will contain 24 words.
defaultEntropyLength :: Int
defaultEntropyLength = 32

getEntropy :: IO Entropy
getEntropy = E.getEntropy defaultEntropyLength
