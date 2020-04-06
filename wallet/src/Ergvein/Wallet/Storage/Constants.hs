module Ergvein.Wallet.Storage.Constants (
    gapLimit
  ) where

-- | Defines the number of generated private / publick keys in excess of the number that was used.
-- Suppose you have N keys used in storage, then wallet application will tend to keep N + gapLimit keys.
gapLimit :: Int
gapLimit = 20
