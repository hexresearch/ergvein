module Data.Bitstream.CTest where

import           Data.Foldable (traverse_)
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
