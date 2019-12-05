{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Ergo.Headers where

import Test.Tasty.Hspec

spec_prelude :: Spec
spec_prelude = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
