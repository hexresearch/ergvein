{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module ProtocolTest where

--------------------------------------------------------------------------
-- imports

import Control.Monad (replicateM)
import ProtocolTest.Generators
import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty.HUnit

import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types
import Ergvein.Types.Currency (Fiat(..))
import Ergvein.Types.Fees

import qualified Data.Vector.Unboxed        as UV
import qualified Data.Vector                as V
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Base16     as B16

--------------------------------------------------------------------------
-- Serialize-deserialize helpers

serializeMessage :: Message -> BL.ByteString
serializeMessage = BB.toLazyByteString . messageBuilder

deserializeMessage :: BS.ByteString -> Either String Message
deserializeMessage bs = flip AP.parseOnly bs $ messageParser . msgType =<< messageHeaderParser

serializeScanBlock :: ScanBlock -> BL.ByteString
serializeScanBlock = BB.toLazyByteString . snd . scanBlockBuilder

deserializeScanBlock :: BS.ByteString -> Either String ScanBlock
deserializeScanBlock = AP.parseOnly versionBlockParser

serializeMessageHeader :: MessageHeader -> BL.ByteString
serializeMessageHeader MessageHeader{..} = BB.toLazyByteString $ messageBase msgType msgSize (BB.byteString "")

deserializeMessageHeader :: BS.ByteString -> Either String MessageHeader
deserializeMessageHeader = AP.parseOnly messageHeaderParser

--------------------------------------------------------------------------
-- Special case only for implemented messages

--------------------------------------------------------------------------
-- Serialize-deserialize

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
      bytes = "042600fd931f31323334353637383132333435363738313233343536373831323334353637380461626364"
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
