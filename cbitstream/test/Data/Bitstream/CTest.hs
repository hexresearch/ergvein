{-# LANGUAGE BinaryLiterals  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Bitstream.CTest where

import           Control.Monad
import           Data.Foldable (traverse_)
import           Data.Word
import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

import qualified Data.Bitstream.C as BS

spec_basicTests :: Spec
spec_basicTests = describe "basic tests" $ do
  it "creates empty" $ do
    bs <- BS.empty 8
    isnull <- BS.null bs
    isnull `shouldBe` True
  it "singleton is not empty" $ do
    bs <- BS.pack [True]
    isnull <- BS.null bs
    isnull `shouldBe` False
  it "pack empty" $ do
    bs <- BS.pack []
    isnull <- BS.null bs
    isnull `shouldBe` True
  it "pack unpack empty" $ do
    bs <- BS.pack []
    res <- BS.unpack bs
    res `shouldBe` []
  it "bits empty" $ do
    bs <- BS.empty 8
    res <- BS.bits bs
    res `shouldBe` 0
  it "bits pack empty" $ do
    bs <- BS.pack []
    res <- BS.bits bs
    res `shouldBe` 0
  it "bits pack trivial" $ do
    bs <- BS.pack [False, True]
    res <- BS.bits bs
    res `shouldBe` 2
  let packUnpackTrivial as = it ("pack unpack " ++ show as) $ do
        bs <- BS.pack as
        res <- BS.unpack bs
        res `shouldBe` as
  traverse_ packUnpackTrivial [
      [False]
    , [True]
    , [False, True]
    , [True, False]
    , [True, True]
    , [True, False, False]
    , [True, False, True]
    ]
  it "replicateBits 5 True" $ do
    bs <- BS.replicateBits 5 True =<< BS.empty 1
    res <- BS.unpack bs
    res `shouldBe` replicate 5 True
  it "replicateBits 8 False" $ do
    bs <- BS.replicateBits 8 False =<< BS.empty 1
    res <- BS.unpack bs
    res `shouldBe` replicate 8 False
  it "replicateBits 16 True (realloc)" $ do
    bs <- BS.replicateBits 16 True =<< BS.empty 1
    res <- BS.unpack bs
    res `shouldBe` replicate 16 True
  it "writeNBits 3 0b1111" $ do
    bs <- BS.empty 1
    bs' <- BS.writeNBits 3 0b1111 bs
    res <- BS.unpack bs'
    res `shouldBe` [True, True, True]
  it "writeNBits 3 0b1111 >> writeNBits 3 0b1001" $ do
    bs <- BS.empty 1
    bs' <- BS.writeNBits 3 0b1001 =<< BS.writeNBits 3 0b1111 bs
    res <- BS.unpack bs'
    res `shouldBe` [True, True, True, False, False, True]
  it "writeBits 524388" $ do
    bs <- BS.empty 1
    bs' <- BS.writeBits (524388 :: Word64) bs
    res :: Word64 <- BS.readBits bs'
    res `shouldBe` 524388
  it "readNBits 3 0b1111" $ do
    bs <- BS.empty 1
    bs' <- BS.writeNBits 3 0b1111 bs
    res <- BS.readNBits 3 bs'
    res `shouldBe` 0b111
  it "drop 1" $ do
    bs <- BS.pack [True, True, False, True]
    BS.drop 1 bs
    res <- BS.unpack bs
    res `shouldBe` [True, False, True]
  it "countWhile id" $ do
    bs <- BS.pack [True, True, False, True]
    res <- BS.countWhile id bs
    leftover <- BS.unpack bs
    res `shouldBe` 2
    leftover `shouldBe` [True]
  it "reallocates" $ do
    let ws = [12,28,3,23,4,15,15,31,11,32,5,19,0,6,0,17,2] :: [Word64]
    bs <- BS.empty 16
    bs' <- foldM (flip BS.writeBits) bs ws
    rw <- replicateM (length ws) $ BS.readBits bs'
    isnull <- BS.null bs'
    rw `shouldBe` ws
    isnull `shouldBe` True


prop_encodingDecodingWord :: Small Word64 -> Property
prop_encodingDecodingWord (Small w) = idempotentIOProperty $ do
  bs <- BS.empty 8
  bs' <- BS.writeBits w bs
  rw <- BS.readBits bs'
  isnull <- BS.null bs'
  pure $ rw == w && isnull

prop_encodingDecodingWords :: [Small Word64] -> Property
prop_encodingDecodingWords ws = idempotentIOProperty $ do
  let ws' = fmap (\(Small w) -> w) ws
  bs <- BS.empty 16
  bs' <- foldM (flip BS.writeBits) bs ws'
  rw <- replicateM (length ws) $ BS.readBits bs'
  isnull <- BS.null bs'
  pure $ rw == ws' && isnull
