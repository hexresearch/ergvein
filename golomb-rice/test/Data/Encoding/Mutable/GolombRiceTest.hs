{-# LANGUAGE CPP #-}
module Data.Encoding.Mutable.GolombRiceTest where

import           Data.Word
import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

#ifdef CBITSTREAM

import           Data.Foldable

import qualified Data.Bitstream.C              as BS
import           Data.Encoding.GolombRice.Strict.Mutable.Internal
                                               as G
p :: Int
p = 19

type GRWord = G.GolombRice Word64

spec_basicEncoding :: Spec
spec_basicEncoding = describe "basic test vectors" $ do
  it "empty stream is empty" $ do
    res <- G.null =<< (G.empty p 8 :: IO GRWord)
    res `shouldBe` True
  it "non empty stream is not empty" $ do
    res <- G.null =<< (G.singleton p 0 :: IO GRWord)
    res `shouldBe` False
  it "encodes long number" $ do
    let a = 524388
    res <- G.read =<< (G.singleton p a :: IO GRWord)
    res `shouldBe` a

spec_encodingCheck :: Spec
spec_encodingCheck = describe "encoded bits are correct" $ do
  let showBits = fmap $ \v -> if v then '1' else '0'
  let check i is = it ("encodes " <> show i <> " as " <> showBits is) $ do
        s <- BS.unpack =<< encodeWord 2 i =<< BS.empty 1
        s `shouldBe` is
  traverse_
    (uncurry check)
    [ (0 , [False, False, False])
    , (1 , [False, False, True])
    , (2 , [False, True, False])
    , (3 , [False, True, True])
    , (4 , [True, False, False, False])
    , (5 , [True, False, False, True])
    , (6 , [True, False, True, False])
    , (7 , [True, False, True, True])
    , (8 , [True, True, False, False, False])
    , (9 , [True, True, False, False, True])
    , (10, [True, True, False, True, False])
    ]

prop_encodingDecodingWord :: Small Word64 -> Property
prop_encodingDecodingWord (Small w) = idempotentIOProperty $ do
  s0 <- BS.empty 8
  s1 <- encodeWord p w s0
  rw <- decodeWord p s1
  e <- BS.null s1
  pure $ rw == w && e

prop_singletonHeadWord :: Small Word64 -> Property
prop_singletonHeadWord (Small w) = idempotentIOProperty $ do
  res <- G.read =<< G.singleton p w
  pure $ res == w

prop_encodingDecodingWords :: [Small Word64] -> Property
prop_encodingDecodingWords wss = idempotentIOProperty $ do
  let ws = fmap (\(Small a) -> a) wss
  res <- G.toList =<< G.fromList p ws
  pure $ res == ws

prop_encodingDecodingWordsBs :: [Small Word64] -> Property
prop_encodingDecodingWordsBs wss = idempotentIOProperty $ do
  let ws = fmap (\(Small a) -> a) wss
  gs1 <- G.fromList p ws
  bs1 <- G.toByteString gs1
  gs2 :: GRWord <- G.fromByteString (golombRiceP gs1) bs1
  bs2 <- G.toByteString gs2
  pure $ bs1 == bs2
#else

spec_basicEncoding :: Spec
spec_basicEncoding = pure ()

spec_encodingCheck :: Spec
spec_encodingCheck = pure ()

prop_encodingDecodingWord :: Small Word64 -> Bool
prop_encodingDecodingWord = const True

prop_singletonHeadWord :: Small Word64 -> Bool
prop_singletonHeadWord = const True

prop_encodingDecodingWords :: [Small Word64] -> Bool
prop_encodingDecodingWords = const True

prop_encodingDecodingWordsBs :: [Small Word64] -> Property
prop_encodingDecodingWordsBs = const True

#endif
