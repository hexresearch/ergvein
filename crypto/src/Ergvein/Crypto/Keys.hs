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

import Control.Monad           (unless)
import Crypto.Secp256k1        (SecKey)
import Data.ByteString         (ByteString)
import Data.Serialize.Get      (Get, getWord8)
import Data.Serialize.Put      (Putter, putWord8, runPut)
import Network.Haskoin.Address
import Network.Haskoin.Keys

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
