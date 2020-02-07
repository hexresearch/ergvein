module Ergvein.Crypto.PBKDF (
    fastPBKDF2_SHA256
  , defaultPBKDF2Params
  , defaultPBKDF2SaltLength
  ) where

import Crypto.KDF.PBKDF2 (Parameters(..), fastPBKDF2_SHA256)

defaultPBKDF2Params :: Parameters
defaultPBKDF2Params = Parameters {
    iterCounts = 100000
  , outputLength = 32
}

defaultPBKDF2SaltLength :: Int
defaultPBKDF2SaltLength = 32
