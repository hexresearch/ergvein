module VersionTest where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Word
import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types
import ProtocolTest.Generators
import Test.Tasty.HUnit

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Lazy as BSL

import Debug.Trace

decodeVersion :: ByteString -> Either String ProtocolVersion
decodeVersion = AP.parseOnly versionParser

prop_encoderDecoderIdemp :: PVT -> Bool
prop_encoderDecoderIdemp (PVT v) = decodeVersion (mkProtocolVersion v) == Right v

unit_oldVersion :: IO ()
unit_oldVersion = do
  let v = 1 :: Word32
      vbs = BSL.toStrict $ toLazyByteString $ word32LE v
  decodeVersion vbs @?= Right (0, 0, 1)
