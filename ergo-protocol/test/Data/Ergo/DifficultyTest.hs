module Data.Ergo.DifficultyTest where

import Data.Ergo.Difficulty
import Data.Persist
import Data.Word
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.ByteString as BS

prop_nBitsIdempotent :: Integer -> Bool
prop_nBitsIdempotent a = fst (decodeDifficulty (encodeDifficulty (Difficulty a))) == Difficulty a

testDifficulty :: Word32 -> Integer -> IO ()
testDifficulty c a = fst (decodeDifficulty c) @?= Difficulty a

unit_difficulty1 :: IO ()
unit_difficulty1 = testDifficulty 0x01003456 0
