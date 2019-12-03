module Ergvein.Interfaces.Ergo.Scorex.Crypto.Hash where

import Data.ByteString
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S

newtype Digest32 = Digest32 { unDigest32 :: ByteString }

instance Serialize Digest32 where
    get = Digest32 <$> S.getBytes 32
    put = S.putByteString . unDigest32

newtype Digest64 = Digest64 { unDigest64 :: ByteString }

instance Serialize Digest64 where
    get = Digest64 <$> S.getBytes 64
    put = S.putByteString . unDigest64
