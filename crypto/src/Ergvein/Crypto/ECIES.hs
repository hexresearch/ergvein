module Ergvein.Crypto.ECIES(
    Curve_X25519
  , CryptoFailable(..)
  , Point
  , deriveEncrypt
  ) where

import Crypto.ECC          (Curve_X25519, Point)
import Crypto.Error
import Crypto.PubKey.ECIES
