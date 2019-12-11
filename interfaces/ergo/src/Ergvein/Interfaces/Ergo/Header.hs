module Ergvein.Interfaces.Ergo.Header where

-----------------------------------------------------------------------------
-- | The port of:
-- ergo/src/main/scala/org/ergoplatform/modifiers/history/Header.scala
-----------------------------------------------------------------------------

import Control.Monad
import Data.Aeson
import Data.ByteString
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Data.String
import Data.Time
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Ergvein.Aeson

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
} deriving (Eq, Show)

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

instance FromJSON Header where
  parseJSON = withObject "Header" $ \o -> do
    -- version <- c.downField("version").as[Byte]
    version <- o .: "version"  --  Version
    -- parentId <- c.downField("parentId").as[ModifierId]
    parentId <- o .: "parentId"  --  ModifierId
    -- adProofsRoot <- c.downField("adProofsRoot").as[Digest32]
    adProofsRoot <- o .: "adProofsRoot"  --  Digest32
    -- transactionsRoot <- c.downField("transactionsRoot").as[Digest32]
    transactionsRoot <- o .: "transactionsRoot"  --  Digest32
    -- stateRoot <- c.downField("stateRoot").as[ADDigest]
    stateRoot <- o .: "stateRoot"  --  ADDigest
    -- timestamp <- c.downField("timestamp").as[Long]
    timestamp <- o .: "timestamp"  --  Timestamp
    -- extensionHash <- c.downField("extensionHash").as[Digest32]
    extensionRoot <- o .: "extensionHash"  --  Digest32
    -- nBits <- c.downField("nBits").as[Long]
    nBits <- o .: "nBits"  --  NBits
    -- height <- c.downField("height").as[Int]
    height <- o .: "height"  --  Height
    -- votes <- c.downField("votes").as[String]
    votes <- o .: "votes"  --  Votes
    -- solutions <- c.downField("powSolutions").as[AutolykosSolution]
    powSolution <- o .: "powSolutions"  --  AutolykosSolution
--     } yield Header(version, parentId, adProofsRoot, stateRoot,
--       transactionsRoot, timestamp, nBits, height, extensionHash, solutions, Algos.decode(votes).get)
    pure Header {..}
  {-# INLINE parseJSON #-}

newtype Votes = Votes { unVotes :: ByteString }
  deriving (Eq)

instance Show Votes where
    show = show . toHex . unVotes

instance IsString Votes where
    fromString = Votes . fromHex . fromString

instance Serialize Votes where
  -- votes: Array[Byte], //3 bytes
    put = S.putByteString . unVotes
    get = Votes <$> S.getBytes 3

instance ToJSON Votes where
  toJSON = String . toHex . unVotes
  {-# INLINE toJSON #-}

instance FromJSON Votes where
  parseJSON = withText "Votes" $ \v -> do
    bs <- either fail (pure) . fromHexEitherText $ v
    if (BS.length bs == 3)
      then fail "Must be hex representation of bytestring 3 characters long"
      else (pure . Votes $ bs)
  {-# INLINE parseJSON #-}
