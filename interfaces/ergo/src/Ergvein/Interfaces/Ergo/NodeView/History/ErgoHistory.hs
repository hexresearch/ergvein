module Ergvein.Interfaces.Ergo.NodeView.History.ErgoHistory where

import Data.Aeson
import Data.Int
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Data.Word

import Ergvein.Aeson

-- From nodeView/history/ErgoHistory.scala
newtype NBits = NBits { unNBits :: Word32 }
  deriving (Eq, Show)

instance Serialize NBits where
    -- override def serialize(obj: NBits, w: Writer): Unit = { w.putBytes(uint32ToByteArrayBE(obj)) }
    put = putWord32be . unNBits

    -- override def parse(r: Reader): NBits = { readUint32BE(r.getBytes(4)) }
    get = NBits <$> getWord32be

deriveJSON unwrapUnaryOptions ''NBits
