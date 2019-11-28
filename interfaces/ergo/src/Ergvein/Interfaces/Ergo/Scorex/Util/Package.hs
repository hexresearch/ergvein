module Ergvein.Interfaces.Ergo.Scorex.Util.Package where

import Data.HexString as H
import Data.ByteString

import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S

type ModifierId = HexString

bytesToId :: ByteString -> ModifierId
bytesToId = H.fromBytes

idToBytes :: ModifierId -> ByteString
idToBytes = H.toBytes

instance Serialize ModifierId where
    get = bytesToId <$> S.getBytes 32
    put = S.putByteString . idToBytes
