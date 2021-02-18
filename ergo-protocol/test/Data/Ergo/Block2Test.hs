module Data.Ergo.Block2Test where

import Data.ByteString (ByteString)
import Data.Either
import Data.Text (Text, unpack)
import Data.Ergo.Autolykos
import Data.Ergo.Block
import Data.Ergo.Crypto
import Data.Ergo.Difficulty
import Data.Ergo.Modifier
import Data.Persist
import Data.Time
import Data.Time.Clock.POSIX
import Test.Tasty.HUnit

import qualified Data.ByteString.Base16 as B16

import Debug.Trace

unit_blockHeader2Parse :: IO ()
unit_blockHeader2Parse = do
  -- | This test contains simple parser for header version 2

  let mh = traceShowId $ runGet (get :: Get BlockHeader) header2
  assertBool "Block header 418138 parsed" $ isRight mh
  let Right h = mh

  -- property("Header simple parsing - block version 2") {
  --   | real header from mainnet, at 418,838, https://explorer.ergoplatform.com/en/blocks/f46c89e44f13a92d8409341490f97f05c85785fa8d2d2164332cc066eda95c39
  --
  --   val version = 2 : Byte
  let version' = 2

  --   val height = 418138
  let height' = 418138

  --   val parentId = ModifierId @@ "7fbc70ec5913706ddef67bbcdb7700ea5f15dc709012491269c9c7eb545d720c"
  let parentId' = ModifierId (b16 "7fbc70ec5913706ddef67bbcdb7700ea5f15dc709012491269c9c7eb545d720c")

  --   val h = Header(version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits,
  --     height, extensionRoot, powSolution, votes)

  --   h.id shouldBe "f46c89e44f13a92d8409341490f97f05c85785fa8d2d2164332cc066eda95c39"

  --   h.id shouldBe Base16.encode(Blake2b256(h.bytes)) | header id is blake2b256 of its bytes

  --   val bb = ByteBuffer.wrap(h.bytes)

  --   val adProofsRoot = Digest32 @@ Base16.decode("a80bbd4d69b4f017da6dd9250448ef1cde492121fc350727e755c7b7ae2988ad").get
  let adProofsRoot' = Digest32 (b16 "a80bbd4d69b4f017da6dd9250448ef1cde492121fc350727e755c7b7ae2988ad")

  --   val transactionsRoot = Digest32 @@ Base16.decode("141bf3de015c44995858a435e4d6c50c51622d077760de32977ba5412aaaae03").get
  let transactionsRoot' = Digest32 (b16 "141bf3de015c44995858a435e4d6c50c51622d077760de32977ba5412aaaae03")

  --   val stateRoot = ADDigest @@ Base16.decode("995c0efe63744c5227e6ae213a2061c60f8db845d47707a6bff53f9ff1936a9e13").get
  let stateRoot' = Digest33 (b16 "995c0efe63744c5227e6ae213a2061c60f8db845d47707a6bff53f9ff1936a9e13")

  --   val timestamp = 1612465607426L
  let timestamp' = utcTimeFromPosixMilliseconds 1612465607426

  --   val extensionRoot = Digest32 @@ Base16.decode("b1457df896bba9dc962f8e42187e1ac580842f1282c8c7fb9cf9f4cd520d1c07").get
  let extensionRoot' = Digest32 (b16 "b1457df896bba9dc962f8e42187e1ac580842f1282c8c7fb9cf9f4cd520d1c07")

  --   val nBits = 107976917L
  let nBits' = Difficulty 107976917

  --   val votes = Array[Byte](0, 0, 0)
  let votes' = ParamVotes 0 0 0

  --   val pk = base16ToEcPoint("0315345f1fca9445eee5df74759d4c495094bcfc82a2831b26fca6efa599b509de")
  --   val w = AutolykosSolution.wForV2
  --   val n = Base16.decode("1b95db2168f95fda").get
  --   val d = AutolykosSolution.dForV2
  --   val powSolution = AutolykosSolution(pk, w, n, d)
  let powSolution' = AutolykosSolution {
          minerPubKey = ecPoint "0315345f1fca9445eee5df74759d4c495094bcfc82a2831b26fca6efa599b509de"
        , oneTimePubKey = wForV2
        , nonce = b16 "1b95db2168f95fda"
        , distance = dForV2
        }

  let h' = BlockHeader {
          version          = version'          -- !BlockVersion
        , parentId         = parentId'         -- !ModifierId        -- ^ id of a parent block header
        , adProofsRoot     = adProofsRoot'     -- !Digest32          -- ^ digest of UTXO set transformation proofs
        , transactionsRoot = transactionsRoot' -- !Digest32          -- ^ Merkle tree digest of transactions in the block (BlockTransactions section)
        , stateRoot        = stateRoot'        -- !ADDigest          -- ^ AVL+ tree digest of UTXO set (after the block). Extra byte with tree height
        , timestamp        = timestamp'        -- !UTCTime           -- ^ block generation time reported by a miner
        , extensionRoot    = extensionRoot'    -- !Digest32          -- ^ Merkle tree digest of the extension section of the block
        , nBits            = nBits'            -- !Difficulty        -- ^ difficulty encoded
        , height           = height'           -- !Word32            -- ^ height of the block (genesis block height == 1)
        , votes            = votes'            -- !ParamVotes        -- ^ votes for changing system parameters
        , powSolution      = powSolution'      -- !AutolykosSolution -- ^ solution for the proof-of-work puzzle
        }
  print $ B16.encode $ runPut . put $ h'



  --   | read block version
  --   val versionParsed = getByte(bb)
  --   versionParsed shouldBe version
  version h @?= version'

  --   | read parent id, 32 bytes
  --   val parentIdParsed = getBytes(bb, 32)
  --   Base16.encode(parentIdParsed) shouldBe parentId
  parentId h @?= parentId'

  --   | read authenticating hash of state transformation correctness proofs, 32 bytes
  --   val adProofsRootParsed = getBytes(bb, 32)
  --   adProofsRootParsed.toIndexedSeq shouldBe adProofsRoot.toIndexedSeq
  adProofsRoot h @?= adProofsRoot'

  --   | read transactions Merkle tree root, 32 bytes
  --   val transactionsRootParsed = getBytes(bb, 32)
  --   transactionsRootParsed.toIndexedSeq shouldBe transactionsRoot.toIndexedSeq
  transactionsRoot h @?= transactionsRoot'

  --   | read UTXO state AVL+ tree root + height, 33 bytes
  --   val stateRootParsed = getBytes(bb, 33)
  --   stateRootParsed.toIndexedSeq shouldBe stateRoot.toIndexedSeq
  stateRoot h @?= stateRoot'

  --   val timestampParsed = getULong(bb) | timestamp, up to 8 bytes
  --   timestampParsed shouldBe timestamp
  fromTime (timestamp h) @?= fromTime timestamp'

  --   | read Merkle tree root of block extension data, 32 bytes
  --   val extensionRootParsed = getBytes(bb, 32)
  --   extensionRootParsed.toIndexedSeq shouldBe extensionRoot.toIndexedSeq
  extensionRoot h @?= extensionRoot'

  --   | read difficulty encoded in Bitcoin nBits format, https://bitco.in/en/developer-reference#target-nbits
  nBits h @?= nBits'

  --   val heightParsed = getULong(bb) | up to 4 bytes
  --   heightParsed shouldBe height

  --   | read miner votes for protocol parameters update
  --   val votesParsed = getBytes(bb, 3)
  --   votesParsed.toIndexedSeq shouldBe votes.toIndexedSeq
  votes h @?= votes'


  --   | Block version V2 specific field, contains length of additional data
  --   val additionalFieldsLength = getUByte(bb)
  --   additionalFieldsLength shouldBe 0

  --   | read PoW solution, no "w" and "d" in block version 2

  --   val pkParsed = getBytes(bb, 33)
  --   groupElemFromBytes(pkParsed) shouldBe pk

  --   val nonceParsed = getBytes(bb, 8)
  --   nonceParsed.toIndexedSeq shouldBe n.toIndexedSeq

  --   bb.remaining() shouldBe 0

  pure ()

  where
      b16 = fst . B16.decode

      ecPoint :: Text -> EcPointType
      ecPoint s = either (error $ "Failed to parse public key " ++ unpack s) id . decodeEcPointType $ s

      -- dlogGroup :: object SecP256K1 extends BcDlogGroup[SecP256K1Point](CustomNamedCurves.getByName("secp256k1"))
      -- wForV2 = CryptoConstants.dlogGroup.generator
      wForV2 :: EcPointType
      wForV2 = ecPoint "026d7b267c33120d15c267664081a6b77a6dcae6b35147db2c3e1195573119cb14"

      dForV2 :: Integer
      dForV2 = 0

utcTimeFromPosixMilliseconds :: POSIXTime -> UTCTime
utcTimeFromPosixMilliseconds = posixSecondsToUTCTime . (/1000)

toTime :: String -> UTCTime
toTime = parseTimeOrError False defaultTimeLocale "%H:%M:%S %d.%m.%Y %z"

fromTime :: UTCTime -> String
fromTime = formatTime defaultTimeLocale "%H:%M:%S %d.%m.%Y %z"

-- | Block header from mainnet, at 418,838, https://explorer.ergoplatform.com/en/blocks/f46c89e44f13a92d8409341490f97f05c85785fa8d2d2164332cc066eda95c39
header2 :: ByteString
header2 = fst $ B16.decode "027fbc70ec5913706ddef67bbcdb7700ea5f15dc709012491269c9c7eb545d720ca80bbd4d69b4f017da6dd9250448ef1cde492121fc350727e755c7b7ae2988ad141bf3de015c44995858a435e4d6c50c51622d077760de32977ba5412aaaae03995c0efe63744c5227e6ae213a2061c60f8db845d47707a6bff53f9ff1936a9e1382a6c2f3f62e04066f98b1457df896bba9dc962f8e42187e1ac580842f1282c8c7fb9cf9f4cd520d1c07dac219000000000315345f1fca9445eee5df74759d4c495094bcfc82a2831b26fca6efa599b509de026d7b267c33120d15c267664081a6b77a6dcae6b35147db2c3e1195573119cb141b95db2168f95fda00"
