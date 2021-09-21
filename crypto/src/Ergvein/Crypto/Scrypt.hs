module Ergvein.Crypto.Scrypt (
    scryptGenerateKey
  , scryptDefaultParams
  ) where

import Data.ByteArray

import Crypto.KDF.Scrypt

scryptGenerateKey :: (ByteArrayAccess password, ByteArrayAccess salt, ByteArray output) => Parameters -> password -> salt -> output
scryptGenerateKey = generate

scryptDefaultParams :: Parameters
scryptDefaultParams = Parameters {
    n = 65536
  , r = 8
  , p = 1
  , outputLength = 32
}
