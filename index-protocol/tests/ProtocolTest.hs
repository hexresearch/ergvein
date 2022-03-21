{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module ProtocolTest where

--------------------------------------------------------------------------
-- imports

import Codec.Compression.GZip
import Control.Monad (replicateM)
import Data.Word
import ProtocolTest.Generators
import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty.HUnit

import Ergvein.Index.Protocol.Deserialization  as Des
import Ergvein.Index.Protocol.Serialization    as Ser
import Ergvein.Index.Protocol.Types
import Ergvein.Types.Currency (Fiat(..),Currency)
import Ergvein.Types.Fees
import Ergvein.Text(hex2bs, bs2Hex)

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy       as BL
import qualified Data.List as L
import qualified Data.Map.Strict            as M
import qualified Data.Serialize.Put         as P
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as UV

--------------------------------------------------------------------------
-- Serialize-deserialize helpers

serializeMessage :: Message -> BL.ByteString
serializeMessage = P.runPutLazy . messageBuilder

deserializeMessage :: BS.ByteString -> Either String Message
deserializeMessage bs = flip AP.parseOnly bs $ messageParser . msgType =<< messageHeaderParser

serializeScanBlock :: ScanBlock -> BL.ByteString
serializeScanBlock = P.runPutLazy . snd . scanBlockBuilder

deserializeScanBlock :: BS.ByteString -> Either String ScanBlock
deserializeScanBlock = AP.parseOnly versionBlockParser

serializeMessageHeader :: MessageHeader -> BL.ByteString
serializeMessageHeader MessageHeader{..} = P.runPutLazy $ messageBase msgType msgSize (P.putByteString "")

deserializeMessageHeader :: BS.ByteString -> Either String MessageHeader
deserializeMessageHeader = AP.parseOnly messageHeaderParser

--------------------------------------------------------------------------
-- Special case only for implemented messages

--------------------------------------------------------------------------
-- Serialize-deserialize

prop_lenbs_builder :: BS.ByteString -> Bool
prop_lenbs_builder bs = either (const False) (bs ==) decomp
  where
    comp = P.runPutLazy $ snd $ lenBsBuilder bs
    decomp = AP.parseOnly parseLenBs $ BL.toStrict comp

prop_encdec_MsgHeader_Eq mh = either (const False) (mh ==) decMsg
  where
    encMsg = serializeMessageHeader mh
    decMsg = deserializeMessageHeader $ BL.toStrict encMsg

prop_encdec_Msg_Valid msg = whenFail dbgPrint $ either (const False) (const True) decMsg
  where
    encMsg = serializeMessage msg
    decMsg = deserializeMessage $ BL.toStrict encMsg
    dbgPrint = do
      print $ show msg
      print $ "encMsg: " <> (show $ BL.unpack encMsg)
      print $ "decMsg: " <> (show decMsg)

prop_encdec_Msg_Eq msg = whenFail dbgPrint $ either (const False) (msg ==) decMsg
  where
    encMsg = serializeMessage msg
    decMsg = deserializeMessage $ BL.toStrict encMsg
    dbgPrint = do
      print $ show msg
      print $ "encMsg: " <> (show $ BL.unpack encMsg)
      print $ "decMsg: " <> (show decMsg)

prop_encdec_ScanBlock_Valid sb = either (const False) (const True) decMsg
  where
    encMsg = serializeScanBlock sb
    decMsg = deserializeScanBlock $ BL.toStrict encMsg

prop_encdec_ScanBlock_Eq sb = either (const False) (sb ==) decMsg
  where
    encMsg = serializeScanBlock sb
    decMsg = deserializeScanBlock $ BL.toStrict encMsg

prop_encdec_version v = either (const False) (v == ) decVer
  where
    encVer = mkProtocolVersion $ unPVT v
    decVer = fmap PVT $ AP.parseOnly versionParser encVer

testMessageHex :: Message -> BS.ByteString -> IO ()
testMessageHex v bytes = do
  let vbs = B16.encode $ BL.toStrict $ serializeMessage v
  vbs @?= bytes
  deserializeMessage (fst $ B16.decode bytes) @?= Right v

unit_versionMsg :: IO ()
unit_versionMsg = do
  let v = MVersion $ Version {
          versionVersion = (1, 2, 4)
        , versionTime = 1615562102
        , versionNonce = 0x0706050403020100
        , versionScanBlocks = [
              ScanBlock {
                scanBlockCurrency = BTC
              , scanBlockVersion = (1, 2, 4)
              , scanBlockScanHeight = 674299
              , scanBlockHeight = 300000
              }
            , ScanBlock {
                scanBlockCurrency = ERGO
              , scanBlockVersion = (4, 1, 0)
              , scanBlockScanHeight = 374299
              , scanBlockHeight = 200000
              }
          ]
        }
      bytes = "00330100200476854b60000000000001020304050607020001002004fefb490a00fee09304000200001010fe1bb60500fe400d0300"
  testMessageHex v bytes

unit_versionMsg2 :: IO ()
unit_versionMsg2 = do
  let v = MVersion $ Version {
          versionVersion = (2, 0, 0)
        , versionTime = 1615797606
        , versionNonce = 12025291936665732026
        , versionScanBlocks = []
        }
      bytes = "001500000008661d4f6000000000ba8354bd4d6be2a600"
  testMessageHex v bytes

unit_versionAckMsg :: IO ()
unit_versionAckMsg = do
  let v = MVersionACK VersionACK
      bytes = "01"
  testMessageHex v bytes

unit_pingMsg :: IO ()
unit_pingMsg = do
  let v = MPing 424143
      bytes = "0b08cf78060000000000"
  testMessageHex v bytes

unit_pongMsg :: IO ()
unit_pongMsg = do
  let v = MPong 424143
      bytes = "0c08cf78060000000000"
  testMessageHex v bytes

unit_rejectMsg :: IO ()
unit_rejectMsg = do
  let v = MReject Reject {
          rejectId = MFiltersRequestType
        , rejectMsgCode = InternalServerError
        , rejectMsg = "Something went wrong"
        }
      bytes = "0a17020214536f6d657468696e672077656e742077726f6e67"
  testMessageHex v bytes

unit_filtersReqMsg :: IO ()
unit_filtersReqMsg = do
  let v = MFiltersRequest FilterRequest {
          filterRequestMsgCurrency = BTC
        , filterRequestMsgStart = 445123
        , filterRequestMsgAmount = 2000
        }
      bytes = "020900fec3ca0600fdd007"
  testMessageHex v bytes

-- Non deterministic compression
unit_filtersRespMsg :: IO ()
unit_filtersRespMsg = do
  let v = MFiltersResponse FilterResponse {
          filterResponseCurrency = BTC
        , filterResponseFilters = [
            BlockFilter "12345678123456781234567812345678" "abcd"
          , BlockFilter "22345678123456781234567812345678" "ffff"
          ]
        }
      bytes = "032b00021f8b080000000000000333343236313533b730c441b3242625a71811529406040096e289844a000000"
  testMessageHex v bytes

unit_filterEventMsg :: IO ()
unit_filterEventMsg = do
  let v = MFiltersEvent FilterEvent {
          filterEventCurrency = BTC
        , filterEventHeight = 8083
        , filterEventBlockId = "12345678123456781234567812345678"
        , filterEventBlockFilter = "abcd"
        }
      bytes = "042900fd931f31323334353637383132333435363738313233343536373831323334353637380461626364"
  testMessageHex v bytes

unit_feeReqMsg :: IO ()
unit_feeReqMsg = do
  let v = MFeeRequest [BTC, DASH]
      bytes = "070302000c"
  testMessageHex v bytes

unit_feeRespMsg :: IO ()
unit_feeRespMsg = do
  let v = MFeeResponse [FeeRespBTC False (FeeBundle (4, 8) (15, 16) (23, 42)), FeeRespGeneric DASH 4 8 15]
      bytes = "080c020004080f10172a0c04080f"
  testMessageHex v bytes

unit_peerReqMsg :: IO ()
unit_peerReqMsg = do
  let v = MPeerRequest PeerRequest
      bytes = "05"
  testMessageHex v bytes

unit_peerRespMsg :: IO ()
unit_peerRespMsg = do
  let v = MPeerResponse $ PeerResponse [
          AddressIpv4 2130706433 8333
        , AddressIpv6 (IpV6 0 0 0 1) 8333
        , AddressOnionV3 "jamie22ezawwi5r3o7lrgsno43jj7vq5en74czuw6wfmjzkhjjryxnid" 9150]
      bytes = "065603007f000001208d0100000000000000000000000000000001208d026a616d69653232657a617777693572336f376c7267736e6f34336a6a37767135656e3734637a75773677666d6a7a6b686a6a7279786e696423be"
  testMessageHex v bytes

unit_peerIntroMsg :: IO ()
unit_peerIntroMsg = do
  let v = MPeerIntroduce $ PeerIntroduce [
          AddressIpv4 2130706433 8333
        , AddressIpv6 (IpV6 0 0 0 1) 8333
        , AddressOnionV3 "jamie22ezawwi5r3o7lrgsno43jj7vq5en74czuw6wfmjzkhjjryxnid" 9150]
      bytes = "095603007f000001208d0100000000000000000000000000000001208d026a616d69653232657a617777693572336f376c7267736e6f34336a6a37767135656e3734637a75773677666d6a7a6b686a6a7279786e696423be"
  testMessageHex v bytes

unit_ratesReqMsg :: IO ()
unit_ratesReqMsg = do
  let v = MRatesRequest $ RatesRequest [
          (BTC, [USD, RUB])
        , (DASH, [EUR, RUB]) ]
      bytes = "0d0902000200020c020102"
  testMessageHex v bytes

unit_ratesRespMsg :: IO ()
unit_ratesRespMsg = do
  let v = MRatesResponse $ RatesResponse [
          (BTC, [(USD, 65003.23), (RUB, 350000.42)])
        , (DASH, [(EUR, 0.12), (RUB, 0.01)]) ]
      bytes = "0e2902000200e32f63000000000002ea0e1602000000000c02010c00000000000000020100000000000000"
  testMessageHex v bytes

unit_full_filter_inv :: IO ()
unit_full_filter_inv = do
  let v = MFullFilterInv FullFilterInv
      bytes = "0f"
  testMessageHex v bytes

unit_get_full_filter :: IO ()
unit_get_full_filter = do
  let v = MGetFullFilter GetFullFilter
      bytes = "10"
  testMessageHex v bytes

unit_get_mempool_filters :: IO ()
unit_get_mempool_filters = do
  let v = MGetMemFilters GetMemFilters
      bytes = "12"
  testMessageHex v bytes

unit_get_mempool :: IO ()
unit_get_mempool = do
  let v = MGetMempool $ GetMempool $ V.fromList [(72,12)]
      bytes = "140301480c"
  testMessageHex v bytes

unit_full_filter :: IO ()
unit_full_filter = do
  let mf = MempoolFilter $ hex2bs "13ef4d423ac40d6e5e287611d82f69b1d75b22e30f3a78a9fe0985f45c45757a2efc71f20f491be782c84926941d40499d3238"
  let v = MFullFilter mf
      bytes = "11343313ef4d423ac40d6e5e287611d82f69b1d75b22e30f3a78a9fe0985f45c45757a2efc71f20f491be782c84926941d40499d3238"
  testMessageHex v bytes

unit_mempool_filters :: IO ()
unit_mempool_filters = do
  let ft = FilterTree $ M.fromList $ [
          ((8, 192), MempoolFilter $ hex2bs "00"),
          ((18, 192), MempoolFilter $ hex2bs "00"),
          ((54, 192), MempoolFilter $ hex2bs "00"),
          ((195, 192), MempoolFilter $ hex2bs "00"),
          ((9, 128), MempoolFilter $ hex2bs "022d7f2b50e000"),
          ((248, 0), MempoolFilter $ hex2bs "00"),
          ((72, 64), MempoolFilter $ hex2bs "00"),
          ((136, 64), MempoolFilter $ hex2bs "00"),
          ((10, 0), MempoolFilter $ hex2bs "00"),
          ((110, 64), MempoolFilter $ hex2bs "031b48714c5a3d93d0"),
          ((209, 192), MempoolFilter $ hex2bs "02a49bfc5e31c0"),
          ((121, 64), MempoolFilter $ hex2bs "0189ece0"),
          ((231, 64), MempoolFilter $ hex2bs "00"),
          ((13, 0), MempoolFilter $ hex2bs "00"),
          ((39, 0), MempoolFilter $ hex2bs "00"),
          ((213, 192), MempoolFilter $ hex2bs "039776c95c592b27d0"),
          ((98, 128), MempoolFilter $ hex2bs "01982900"),
          ((171, 128), MempoolFilter $ hex2bs "014f0dd0"),
          ((247, 0), MempoolFilter $ hex2bs "00"),
          ((39, 128), MempoolFilter $ hex2bs "00"),
          ((113, 64), MempoolFilter $ hex2bs "00"),
          ((150, 192), MempoolFilter $ hex2bs "030a2d1d7494236508"),
          ((95, 0), MempoolFilter $ hex2bs "032704d4082c545300"),
          ((181, 128), MempoolFilter $ hex2bs "031ddc812f62d786d8"),
          ((202, 0), MempoolFilter $ hex2bs "00"),
          ((104, 192), MempoolFilter $ hex2bs "00"),
          ((201, 64), MempoolFilter $ hex2bs "00"),
          ((31, 128), MempoolFilter $ hex2bs "00"),
          ((29, 0), MempoolFilter $ hex2bs "00"),
          ((114, 192), MempoolFilter $ hex2bs "0240f91b4f8b80"),
          ((162, 64), MempoolFilter $ hex2bs "0253954696ad")
        ]

  let v = MMemFilters ft
      bytes = "13c51f08c00100098007022d7f2b50e0000a0001000d00010012c001001d0001001f800100270001002780010036c00100484001005f0009032704d4082c5453006280040198290068c001006e4009031b48714c5a3d93d07140010072c0070240f91b4f8b807940040189ece08840010096c009030a2d1d7494236508a240060253954696adab8004014f0dd0b58009031ddc812f62d786d8c3c00100c9400100ca000100d1c00702a49bfc5e31c0d5c009039776c95c592b27d0e7400100f7000100f8000100"
  testMessageHex v bytes

unit_compression_test :: IO ()
unit_compression_test = do
  let v = "123aef32f21a2e7d9c"
      bytes = hex2bs "1f8b08000000000000ff13b27a6ff4494aaf760e004c46c39009000000"
      v2 = bs2Hex $ BL.toStrict $ decompress $ BL.fromStrict bytes
  v @=? v2

unit_test_mempool_chunk_decompress :: IO ()
unit_test_mempool_chunk_decompress = do
  let tx = hex2bs "02000000000101261560a27330e73b46351ac349ff35136f614d4dfdfb3a108fa85c140a1c61a901000000171600149fd77bca5b9369478c80dc5c5cc4101f7baf5a95feffffff0254c410000000000017a914ba906b3da20467de78552d0c089e3754f49f62688740420f000000000017a9140f912a6fc7ba91305934dba0ef566cbfc62fd2218702473044022045d75032c9f3806939ff10ffd79a040bdcbece2f90cb1dc95e3a3b7cf109da1e022012a37cc4fee1ff9ae19c6adf7d0bc84a122b5ce33d5c43bebff52a6796d512340121025609c093b93e3d4a003ebb0ec8e58700d12e6f05c0c1096f18ba3ef8ff931fca260d1b00"
  let msg = MMempoolChunk $ MempoolChunk (9, 128) $ V.fromList [tx,tx,tx,tx,tx,tx]
  let bytesText =  "15fd42010980061f8b08000000000000ffedcfcd4bc2600006f0772f062d2bc75a197da9170f46394289948de84b12840e265423b28b59c10e5d020dbced12a48c3a4864d8254f9d5ba046a3d4202888414921111d232a0c22dfecd03f217baecfc303bf3204d56098b97521b946bf3827ec1d6793c8dec2fb3d9e9f6f07b175c8510d5dfe14565de9db00955042853931e8da8c14394e260ca1a3d9ed0a42087a65e2ef09e85394145d61929ac0c3fa745f63fddea0f723b1b8240c8fe8fe7b5dccc25f48317ac676bfffea5bcd9c5b6f4c0274d163d038ae4c0de4df23c121442025aed116d357d6e865777edee10cbfe1773dd0481e84e54a09c54bbbcb8f1bda9c9bece59e186e349df9b404766e491b66823e3c2b1eb38c1bb027cdb967015cf7f375d9539c6f97d82f241a0ae6a64e5056ddaa5b75abee1a73ff025b13a09cd0050000"
  let msg' = deserializeMessage $ hex2bs bytesText
  Right msg @=? msg'

unit_test_mempool_chunk :: IO ()
unit_test_mempool_chunk = do
  let tx = hex2bs "02000000000101261560a27330e73b46351ac349ff35136f614d4dfdfb3a108fa85c140a1c61a901000000171600149fd77bca5b9369478c80dc5c5cc4101f7baf5a95feffffff0254c410000000000017a914ba906b3da20467de78552d0c089e3754f49f62688740420f000000000017a9140f912a6fc7ba91305934dba0ef566cbfc62fd2218702473044022045d75032c9f3806939ff10ffd79a040bdcbece2f90cb1dc95e3a3b7cf109da1e022012a37cc4fee1ff9ae19c6adf7d0bc84a122b5ce33d5c43bebff52a6796d512340121025609c093b93e3d4a003ebb0ec8e58700d12e6f05c0c1096f18ba3ef8ff931fca260d1b00"
  let msg = MMempoolChunk $ MempoolChunk (9, 128) $ V.fromList [tx,tx,tx,tx,tx,tx]
  let bytes =  "15fd1c010980061f8b0800000000000003fbcec400048c8c6aa2098b8a0d9e5bbb994a1df6fc6f2a9c9fe8ebfbf7b79540ff8a18112e99c4958c4055e2620c22f3af579f8a9e9ce9ded3702726e688807cf5faa8a9fffeffffcf14724400641283f84a915d13b26d17b1a4dfab08d5e5e198671ef2657e5246bb83133f4c9e7fa256fef15d130d224d6e2f781f96b3ff98fe25c576267703172605d7eb0146273f37645afe17f87f7d160bf79d7de7f4279c963d1967655df391f3961c9382d0e29a23ff1efe9ff5704ed6fd5aee135e42da318f6d639cf7edffaa953eedaa9009a3225318e781c93bed6cbd18ec76f39d78dace70512f9ff5c041ce7c895d763ffe4f963fa5c62bcdf07dd4dfa3fe1ef5f7a8bf8799bf015b13a09cd0050000"
  testMessageHex msg bytes

unit_enc_dec_mempool_chunk :: IO ()
unit_enc_dec_mempool_chunk = do
  let tx = hex2bs "02000000000101261560a27330e73b46351ac349ff35136f614d4dfdfb3a108fa85c140a1c61a901000000171600149fd77bca5b9369478c80dc5c5cc4101f7baf5a95feffffff0254c410000000000017a914ba906b3da20467de78552d0c089e3754f49f62688740420f000000000017a9140f912a6fc7ba91305934dba0ef566cbfc62fd2218702473044022045d75032c9f3806939ff10ffd79a040bdcbece2f90cb1dc95e3a3b7cf109da1e022012a37cc4fee1ff9ae19c6adf7d0bc84a122b5ce33d5c43bebff52a6796d512340121025609c093b93e3d4a003ebb0ec8e58700d12e6f05c0c1096f18ba3ef8ff931fca260d1b00"
  let msg = MMempoolChunk $ MempoolChunk (9, 128) $ V.fromList [tx,tx,tx,tx,tx,tx]
  let msgDec = deserializeMessage $ BL.toStrict $ serializeMessage msg
  Right msg @=? msgDec

unit_enumRountrip :: IO ()
unit_enumRountrip = do
  Right currencies @=? currencies'
  where
    currencies  = [minBound .. maxBound] :: [Currency]
    currencies' = traverse roundtrip currencies
    roundtrip :: Currency -> Either String Currency
    roundtrip = AP.parseOnly enumParser . BL.toStrict . P.runPutLazy . enumBuilder

unit_enumOutOfRange :: IO ()
unit_enumOutOfRange = do
  case badEnum of Left  _ -> pure ()
                  Right _ -> assertFailure "Out of range value shouldn't parse"
  where
    badEnum = AP.parseOnly (enumParser @Currency)
            $ BL.toStrict $ P.runPutLazy $ Ser.varInt (10000 :: Word32)
