module Data.Ergo.VarIntTest where

import Data.Word
import Data.Int
import Data.Ergo.Vlq
import Data.Ergo.ZigZag
import Data.Persist

prop_zigzag32 :: Int32 -> Bool
prop_zigzag32 a = decodeZigZag (encodeZigZag a) == a

prop_zigzag64 :: Int64 -> Bool
prop_zigzag64 a = decodeZigZag (encodeZigZag a) == a

prop_vlq32 :: Word32 -> Bool
prop_vlq32 a = runGet decodeVlq (runPut $ encodeVlq a) == Right a

prop_vlq64 :: Word64 -> Bool
prop_vlq64 a = runGet decodeVlq (runPut $ encodeVlq a) == Right a

prop_varInt32 :: Int32 -> Bool
prop_varInt32 a = runGet decodeVarInt (runPut $ encodeVarInt a) == Right a
