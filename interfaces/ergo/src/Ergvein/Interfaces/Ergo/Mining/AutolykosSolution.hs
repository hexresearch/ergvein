module Ergvein.Interfaces.Ergo.Mining.AutolykosSolution where

import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)

-- case class AutolykosSolution(pk: EcPointType, w: EcPointType, n: Array[Byte], d: BigInt) extends BytesSerializable {
data AutolykosSolution = AutolykosSolution {
  }

instance Serialize AutolykosSolution where
    put s = do
      undefined
    get = do
      undefined
