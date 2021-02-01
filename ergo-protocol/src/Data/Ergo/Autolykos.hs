module Data.Ergo.Autolykos(
    AutolykosSolution(..)
  ) where

import Data.ByteString (ByteString)
import Data.Ergo.BigInt
import Data.Ergo.Crypto
import Data.Persist
import GHC.Generics

data AutolykosSolution = AutolykosSolution
  { minerPubKey   :: !EcPointType
  , oneTimePubKey :: !EcPointType
  , nonce         :: !ByteString -- 8 bytes nonce
  , distance      :: !Integer
  } deriving (Generic, Show, Read, Eq)

instance Persist AutolykosSolution where
  put AutolykosSolution{..} = do
    put minerPubKey
    put oneTimePubKey
    putByteString nonce
    putBigNat distance
  {-# INLINE put #-}
  get = AutolykosSolution
    <$> get
    <*> get
    <*> getBytes 8
    <*> getBigNat
  {-# INLINE get #-}
