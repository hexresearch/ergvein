{-# LANGUAGE TypeApplications #-}
module Data.Encoding.GolombRiceTest where

import           Data.Encoding.GolombRice.Strict as G
import           Data.Word
import           Test.Tasty.Hspec

p :: Int
p = 19

type GRWord = G.GolombRice Word64

spec_basicEncoding :: Spec
spec_basicEncoding = describe "basic test vectors" $ do
  it "empty stream is empty" $ G.null (G.empty p :: GRWord) `shouldBe` True
  it "non empty stream is not empty" $ G.null (G.singleton p 0 :: GRWord) `shouldBe` False
