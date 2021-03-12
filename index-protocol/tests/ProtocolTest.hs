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
      vbs = B16.encode $ BL.toStrict $ serializeMessage v
      bytes = "00000000480000000100200476854b60000000000001020304050607020000000000000001002004fb490a0000000000e09304000000000002000000000010101bb6050000000000400d030000000000"
  vbs @?= bytes
  deserializeMessage (fst $ B16.decode bytes) @?= Right v
