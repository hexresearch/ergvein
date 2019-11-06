module Ergvein.Crypto.ECIES(
    Curve_X25519
  , Point
  , Scalar
  , ECIESPrvKey
  , ECIESPubKey
  , CryptoFailable(..)
  , deriveEncrypt
  , deriveDecrypt
  , encodePoint
  , decodePoint
  , secretKey
  ) where

import Crypto.ECC (Curve_X25519, Point, Scalar, encodePoint, decodePoint)
import Crypto.Error
import Crypto.PubKey.Curve25519 (secretKey)
import Crypto.PubKey.ECIES

type ECIESPrvKey = Scalar Curve_X25519

type ECIESPubKey = Point Curve_X25519
