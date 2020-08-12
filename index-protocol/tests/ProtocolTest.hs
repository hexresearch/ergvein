{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Control.Monad (replicateM)
import Test.QuickCheck
import Test.QuickCheck.Instances
import ProtocolTest.Generators

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Deserialization

import qualified Data.Vector.Unboxed        as UV
import qualified Data.Vector                as V
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.Attoparsec.ByteString as AP

--------------------------------------------------------------------------
-- Serialize-deserialize helpers

serializeMessage :: Message -> BL.ByteString
serializeMessage = BB.toLazyByteString . messageBuilder

deserializeMessage :: BS.ByteString -> AP.Result Message
deserializeMessage bs = flip AP.parse bs $ messageParser . msgType =<< messageHeaderParser

serializeScanBlock :: ScanBlock -> BL.ByteString
serializeScanBlock = BB.toLazyByteString . snd . scanBlockBuilder

deserializeScanBlock :: BS.ByteString -> AP.Result ScanBlock
deserializeScanBlock = AP.parse versionBlockParser

serializeMessageHeader :: MessageHeader -> BL.ByteString
serializeMessageHeader MessageHeader{..} = BB.toLazyByteString $ messageBase msgType msgSize (BB.byteString "")

deserializeMessageHeader :: BS.ByteString -> AP.Result MessageHeader
deserializeMessageHeader = AP.parse messageHeaderParser

--------------------------------------------------------------------------
-- Special case only for implemented messages

--------------------------------------------------------------------------
-- Serialize-deserialize

prop_encdec_MsgHeader_Eq mh = maybe False (mh ==) decMsg
  where
    encMsg = serializeMessageHeader mh
    decMsg = AP.maybeResult $ deserializeMessageHeader $ BL.toStrict encMsg

prop_encdec_Msg_Valid msg = whenFail dbgPrint $ maybe False (const True) decMsg
  where
    encMsg = serializeMessage msg
    decMsg = AP.maybeResult $ deserializeMessage $ BL.toStrict encMsg
    decMsg' = AP.eitherResult $ deserializeMessage $ BL.toStrict encMsg
    dbgPrint = do
      print $ show msg
      print $ "encMsg: " <> (show $ BL.unpack encMsg)
      print $ "decMsg: " <> (show decMsg)
      print $ "decMsg: " <> (show decMsg')

prop_encdec_Msg_Eq msg = whenFail dbgPrint $ maybe False (msg ==) decMsg
  where
    encMsg = serializeMessage msg
    decMsg = AP.maybeResult $ deserializeMessage $ BL.toStrict encMsg
    decMsg' = AP.eitherResult $ deserializeMessage $ BL.toStrict encMsg
    dbgPrint = do
      print $ show msg
      print $ "encMsg: " <> (show $ BL.unpack encMsg)
      print $ "decMsg: " <> (show decMsg)
      print $ "decMsg: " <> (show decMsg')

prop_encdec_ScanBlock_Valid sb = maybe False (const True) decMsg
  where
    encMsg = serializeScanBlock sb
    decMsg = AP.maybeResult $ deserializeScanBlock $ BL.toStrict encMsg

prop_encdec_ScanBlock_Eq sb = maybe False (sb ==) decMsg
  where
    encMsg = serializeScanBlock sb
    decMsg = AP.maybeResult $ deserializeScanBlock $ BL.toStrict encMsg

prop_encdec_MultFilters_Valid bfs = bfs == decMsg
  where
    encMsg = BB.toLazyByteString $ mconcat $ snd $ unzip $ blockFilterBuilder <$> bfs
    decMsg = parseFilters $ BL.toStrict encMsg
--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll

--------------------------------------------------------------------------
-- the end.
