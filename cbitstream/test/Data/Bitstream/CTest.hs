module Data.Bitstream.CTest where

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
