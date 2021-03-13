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

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Deserialization

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
