module Data.Ergo.BigIntTest where

import Data.Ergo.BigInt
import Data.Persist
import Data.Word
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.ByteString as BS

prop_bigNatIdempotent :: NonNegative Integer -> Bool
prop_bigNatIdempotent (NonNegative a) =  runGet getBigNat (runPut $ putBigNat a) == Right a

testEncoding :: Integer -> [Word8] -> IO ()
testEncoding a bs = runPut (putBigNat a) @?= BS.pack bs

unit_bigNatZero :: IO ()
unit_bigNatZero = testEncoding 0 [0]

unit_bigNatOne :: IO ()
unit_bigNatOne = testEncoding 1 [1, 1]

unit_bigNatEdge :: IO ()
unit_bigNatEdge = testEncoding 127 [1, 127]

unit_bigNatTwoBytes :: IO ()
unit_bigNatTwoBytes = testEncoding 128 [1, 0x80]

unit_bigNatTwoBytesEdge :: IO ()
unit_bigNatTwoBytesEdge = testEncoding 255 [1, 0xFF]

unit_bigNatTwoBytes2 :: IO ()
unit_bigNatTwoBytes2 = testEncoding 256 [2, 0x01, 0x00]

unit_bigNatTwoBytes3 :: IO ()
unit_bigNatTwoBytes3 = testEncoding 511 [2, 0x01, 0xFF]

unit_bigNatTwoBytes4 :: IO ()
unit_bigNatTwoBytes4 = testEncoding 512 [2, 0x02, 0x00]

unit_bigNatThreeBytes1 :: IO ()
unit_bigNatThreeBytes1 = testEncoding 265837306 [4, 0x0F, 0xD8, 0x5A, 0xFA]
