module Ergvein.Core.Store.Constants (
    gapLimit
  , initialExternalAddressCount
  , initialInternalAddressCount
  , StorageAlert(..)
  ) where

import Data.Text (Text)
import Sepulcas.Native

-- | Defines the number of generated private / public keys in excess of the number that was used.
-- Suppose you have N keys used in storage, then wallet application will tend to keep N + gapLimit keys.
gapLimit :: Int
gapLimit = 20

initialExternalAddressCount :: Int
initialExternalAddressCount = 20

initialInternalAddressCount :: Int
initialInternalAddressCount = 6

-- | Alerts regarding secure storage system
data StorageAlert
  = SADecodeError Text
  | SALoadedSucc
  | SANativeAlert NativeAlerts
  | SAMnemonicFail Text
  | SACryptoError Text
  | SADecryptError Text
  deriving (Eq, Show)
