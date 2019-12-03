module Ergvein.Interfaces.Ergo.Header where

-----------------------------------------------------------------------------
-- | The port of:
-- ergo/src/main/scala/org/ergoplatform/modifiers/history/Header.scala
-----------------------------------------------------------------------------

import Data.ByteString
import Data.Word
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Data.Time

import Ergvein.Interfaces.Ergo.Mining.AutolykosSolution
import Ergvein.Interfaces.Ergo.NodeView.History.ErgoHistory
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Interfaces.Ergo.Scorex.Crypto.Authds
import Ergvein.Interfaces.Ergo.Scorex.Crypto.Hash
import Ergvein.Interfaces.Ergo.Scorex.Util.Package


data Header = Header {
  version :: Version
, parentId :: ModifierId
, adProofsRoot :: Digest32
, transactionsRoot :: Digest32
, stateRoot :: ADDigest
, timestamp :: Timestamp
, extensionRoot :: Digest32
, nBits :: NBits
, height :: Height
, votes :: Votes
, powSolution :: AutolykosSolution
}

instance Serialize Header where
    put Header{..} = do
--   override def serialize(h: Header, w: Writer): Unit = {
--     serializeWithoutPow(h, w)
    -- def serializeWithoutPow(h: Header, w: Writer): Unit = {
    --   w.put(h.version)
        put version
    --   w.putBytes(idToBytes(h.parentId))
        put parentId
    --   w.putBytes(h.ADProofsRoot)
        put adProofsRoot
    --   w.putBytes(h.transactionsRoot)
        put transactionsRoot
    --   w.putBytes(h.stateRoot)
        put stateRoot
    --   w.putULong(h.timestamp)
        put timestamp
    --   w.putBytes(h.extensionRoot)
        put extensionRoot
    --   RequiredDifficulty.serialize(h.nBits, w)
        put nBits
    --   w.putUInt(h.height)
        put height
    --   w.putBytes(h.votes)
        put votes
    -- }
--     AutolykosSolutionSerializer.serialize(h.powSolution, w)
        put powSolution

    get = do
--       override def parse(r: Reader): Header = {
--         val version = r.getByte()
        version <- get
--         val parentId = bytesToId(r.getBytes(32))
        parentId <- get
--         val ADProofsRoot = Digest32 @@ r.getBytes(32)
        adProofsRoot <- get
--         val transactionsRoot = Digest32 @@ r.getBytes(32)
        transactionsRoot <- get
--         val stateRoot = ADDigest @@ r.getBytes(33)
        stateRoot <- get
--         val timestamp = r.getULong()
        timestamp <- get
--         val extensionHash = Digest32 @@ r.getBytes(32)
        extensionRoot <- get
--         val nBits = RequiredDifficulty.parse(r)
        nBits <- get
--         val height = r.getUInt().toIntExact
        height <- get
--         val votes = r.getBytes(3)
        votes <- get
--         val powSolution = AutolykosSolutionSerializer.parse(r)
        powSolution <- get
        pure Header {..}

newtype Votes = Votes { unVotes :: ByteString }

instance Serialize Votes where
  -- votes: Array[Byte], //3 bytes
    put = S.putByteString . unVotes
    get = Votes <$> S.getBytes 3
