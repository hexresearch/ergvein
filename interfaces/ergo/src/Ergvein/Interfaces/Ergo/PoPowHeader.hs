module Ergvein.Interfaces.Ergo.PoPowHeader where

-----------------------------------------------------------------------------
-- | The port of:
-- ergo/src/main/scala/org/ergoplatform/modifiers/history/PoPowHeader.scala
-----------------------------------------------------------------------------

import qualified Data.ByteString as BS
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S

import Ergvein.Interfaces.Ergo.Header
import Ergvein.Interfaces.Ergo.Scorex.Util.Package
import Ergvein.Interfaces.Ergo.Scorex.Util.Serialization.VLQLengthPrefixed


data PoPowHeader = PoPowHeader {
  header :: Header
, interlinks :: [ModifierId]
} deriving (Eq)

instance Serialize PoPowHeader where
    put obj = do
        -- val headerBytes = obj.header.bytes
        -- w.putInt(headerBytes.length)
        -- w.putBytes(headerBytes)
        put . VLQLengthPrefixed $ encode $ header obj
        -- w.putInt(obj.interlinks.size)
        -- obj.interlinks.foreach(x => w.putBytes(idToBytes(x)))
        put . VLQLengthPrefixed $ interlinks obj
    get = do
        -- val headerSize = r.getInt()
        -- val header = HeaderSerializer.parseBytes(r.getBytes(headerSize))
        VLQLengthPrefixed headerbs <- get
        header <- either fail pure $ decode headerbs
        -- val linksQty = r.getInt()
        -- val interlinks = (0 until linksQty).map(_ => bytesToId(r.getBytes(32)))
        VLQLengthPrefixed interlinks <- get
        pure PoPowHeader {..}
