module Ergvein.Wallet.Storage.Constants (
    gapLimit
  , initialExternalAddressCount
  , initialInternalAddressCount
  ) where

-- | Defines the number of generated private / public keys in excess of the number that was used.
-- Suppose you have N keys used in storage, then wallet application will tend to keep N + gapLimit keys.
gapLimit :: Int
gapLimit = 20

initialExternalAddressCount :: Int
initialExternalAddressCount = 20

initialInternalAddressCount :: Int
initialInternalAddressCount = 6