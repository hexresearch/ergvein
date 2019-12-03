module Ergvein.Interfaces.Ergo.Scorex.Core.Block where

import Data.Int
import Data.Word

import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S

import Ergvein.Interfaces.Ergo.Scorex.Util.Package (ModifierId)
import Ergvein.Interfaces.Ergo.Scorex.Util.Serialization.VLQLengthPrefixed

newtype BlockId = BlockId { unBlockId :: ModifierId }
  deriving (Serialize)

newtype Timestamp = Timestamp { unTimestamp :: Word64 } -- Long

instance Serialize Timestamp where
    put = do
        -- w.putULong(h.timestamp)
        put . VLQWord64 . unTimestamp

    get = do
        Timestamp . unVLQWord64 <$> get

newtype Version = Version { unVersion :: Word8 }   -- Byte
  deriving (Serialize)

newtype Height = Height { unHeight :: Word32 }

instance Serialize Height where
    put = do
        -- w.putUInt(h.height)
        put . VLQWord32 . unHeight

    get = do
        Height . unVLQWord32 <$> get
