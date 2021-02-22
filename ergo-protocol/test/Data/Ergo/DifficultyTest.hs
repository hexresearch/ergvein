module Data.Ergo.DifficultyTest where

import Data.Ergo.Difficulty
import Data.Persist
import Data.Word
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.ByteString as BS

import Debug.Trace

prop_nBitsIdempotent :: Integer -> Bool
prop_nBitsIdempotent a = fst (decodeDifficulty (encodeDifficulty (Difficulty a))) == Difficulty a

testDifficulty :: Word32 -> Integer -> IO ()
testDifficulty c a = fst (decodeDifficulty c) @?= Difficulty a

unit_difficulty1 :: IO ()
unit_difficulty1 = testDifficulty 0x01003456 0

unit_difficulty2 :: IO ()
unit_difficulty2 = testDifficulty 0x01123456 0x12

unit_difficulty3 :: IO ()
unit_difficulty3 = testDifficulty 0x02008000 0x80

unit_difficulty4 :: IO ()
unit_difficulty4 = testDifficulty 0x05009234 0x92340000

unit_difficulty5 :: IO ()
unit_difficulty5 = testDifficulty 0x04923456 (-0x12345600)

unit_difficulty6 :: IO ()
unit_difficulty6 = testDifficulty 0x04123456 0x12345600

-- FIXME test failed. Difficulty encoding/decoding should be rewritten
-- unit_difficultyRoundtrip1 :: IO ()
-- unit_difficultyRoundtrip1 = do
--   (runGet get . runPut . put $ d) @?= Right d
--   where
--     d = Difficulty 107976917
