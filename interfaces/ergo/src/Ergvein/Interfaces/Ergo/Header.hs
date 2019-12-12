module Ergvein.Interfaces.Ergo.Header where

-----------------------------------------------------------------------------
-- | The port of:
-- ergo/src/main/scala/org/ergoplatform/modifiers/history/Header.scala
-----------------------------------------------------------------------------

import Data.Aeson as A
import Data.ByteString
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Data.String
import Data.Time

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.Mining.AutolykosSolution
import Ergvein.Interfaces.Ergo.Mining.Difficulty.RequiredDifficulty
import Ergvein.Interfaces.Ergo.Modifiers.History.ModifierType
import Ergvein.Interfaces.Ergo.NodeView.History.ErgoHistory
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Interfaces.Ergo.Scorex.Crypto.Authds
import Ergvein.Interfaces.Ergo.Scorex.Crypto.Hash
import Ergvein.Interfaces.Ergo.Scorex.Util.Package


data Header = Header {
  version :: Version
, parentId :: ModifierId
, adProofsRoot :: AdProofsRoot
, transactionsRoot :: TransactionsRoot
, stateRoot :: ADDigest
, timestamp :: Timestamp
, extensionRoot :: ExtensionRoot
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

instance ToJSON Header where
  toJSON h@Header {..} = object [
      -- "id" -> Algos.encode(h.id).asJson,
      "id" .= toJSON (HexJSON serializedHId)
      -- "transactionsRoot" -> Algos.encode(h.transactionsRoot).asJson,
    , "transactionsRoot" .= toJSON transactionsRoot
      -- "adProofsRoot" -> Algos.encode(h.ADProofsRoot).asJson,
    , "adProofsRoot" .= toJSON adProofsRoot
      -- "stateRoot" -> Algos.encode(h.stateRoot).asJson,
    , "stateRoot" .= toJSON stateRoot
      -- "parentId" -> Algos.encode(h.parentId).asJson,
    , "parentId" .= toJSON parentId
      -- "timestamp" -> h.timestamp.asJson,
    , "timestamp" .= toJSON timestamp
      -- "extensionHash" -> Algos.encode(h.extensionRoot).asJson,
    , "extensionHash" .= toJSON extensionRoot
      -- "powSolutions" -> h.powSolution.asJson,
    , "powSolutions" .= toJSON powSolution
      -- "nBits" -> h.nBits.asJson,
    , "nBits" .= toJSON nBits
      -- "height" -> h.height.asJson,
    , "height" .= toJSON height
      -- "difficulty" -> h.requiredDifficulty.toString.asJson,
    , "difficulty" .= toJSON (requiredDifficulty h)
      -- "version" -> h.version.asJson,
    , "version" .= toJSON version
      -- "votes" -> Algos.encode(h.votes).asJson,
    , "votes" .= toJSON votes
      -- "size" -> h.size.asJson,
    , "size" .= toJSON (BS.length serializedH)
      -- "extensionId" -> Algos.encode(h.extensionId).asJson,
    , "extensionId" .= toJSON (HexJSON $ computeId serializedHId extensionRoot)
      -- "transactionsId" -> Algos.encode(h.transactionsId).asJson,
    , "transactionsId" .= toJSON (HexJSON $ computeId serializedHId transactionsRoot)
      -- "adProofsId" -> Algos.encode(h.ADProofsId).asJson
    , "adProofsId" .= toJSON (HexJSON $ computeId serializedHId adProofsRoot)
    ]
    where
      serializedH = S.encode $ h
      serializedHId = hashFn serializedHId
  {-# INLINE toJSON #-}

-- lazy val requiredDifficulty: Difficulty = RequiredDifficulty.decodeCompactBits(nBits)
requiredDifficulty :: Header -> Difficulty
requiredDifficulty = decodeCompactBits . nBits

computeId :: (HasModifierTypeId a, Serialize a) => ByteString -> a -> ByteString
computeId b a = hashFn $ mconcat [ S.encode (modifierTypeId a), b, S.encode a ]

-- https://github.com/ScorexFoundation/sigmastate-interpreter/blob/98c27448da29d7cb7521d378080d5c52c13b76c3/sigmastate/src/main/scala/org/ergoplatform/settings/ErgoAlgos.scala#L13
-- Blake2b256
hashFn :: ByteString -> ByteString
hashFn = undefined  -- FIXME Blake2b256

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
    bs <- either fail (pure) . fromHexTextEither $ v
    if (BS.length bs == 3)
      then (pure . Votes $ bs)
      else fail "Must be hex representation of bytestring 3 characters long"
  {-# INLINE parseJSON #-}
