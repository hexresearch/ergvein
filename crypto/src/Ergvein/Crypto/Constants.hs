module Ergvein.Crypto.Constants(
    defaultEntropyLength
  ) where

-- | According to the BIP32 the allowed size of entropy is between 16 and 64 bytes (32 bytes is advised).
-- The mnemonic must encode entropy in a multiple of 4 bytes.
-- With 32 bytes of entropy generated mnemonic will contain 24 words.
defaultEntropyLength :: Int
defaultEntropyLength = 32
