module Ergvein.Interfaces.Ergo.Scorex.Crypto.Hash where

import Data.ByteString
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Data.String

import Ergvein.Interfaces.Ergo.Scorex.Util.Package

newtype Digest32 = Digest32 { unDigest32 :: ByteString }
  deriving (Eq)

instance Show Digest32 where
    show = show . toHex . unDigest32

instance IsString Digest32 where
    fromString = Digest32 . fromHex . fromString

instance Serialize Digest32 where
    get = Digest32 <$> S.getBytes 32
    put = S.putByteString . unDigest32

newtype Digest64 = Digest64 { unDigest64 :: ByteString }
  deriving (Eq)

instance Show Digest64 where
    show = show . toHex . unDigest64

instance IsString Digest64 where
    fromString = Digest64 . fromHex . fromString

instance Serialize Digest64 where
    get = Digest64 <$> S.getBytes 64
    put = S.putByteString . unDigest64
