{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
module VersionTest where

import Data.Attoparsec.Binary
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
  decodeVersion vbs @?= Right (0, 0, 4)

unit_sampleVersion1 :: IO ()
unit_sampleVersion1 = do
  let res = AP.parseOnly anyWord32be $ mkProtocolVersion (1, 2, 4)
  res @?= Right 0b0000000100_0000000010_0000000001_00

unit_sampleVersion2 :: IO ()
unit_sampleVersion2 = do
  let res = AP.parseOnly anyWord32be $ mkProtocolVersion (2, 0, 0)
  res @?= Right 0b0000000000_0000000000_0000000010_00
