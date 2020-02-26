{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Ergo.BigNat where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances.Natural ()

import qualified Data.ByteString.Base16 as BS16
import qualified Data.Serialize as S

import Ergvein.Interfaces.Ergo.Common.BigNat

instance Arbitrary BigNat where
  arbitrary = BigNat <$> arbitrary

spec_BigNatSerialize :: Spec
spec_BigNatSerialize = do
  it "Check BigNat serialization" $ do
      (BS16.encode . S.encode . BigNat $ fromIntegral @Integer
          2504075119295696010374311877171546620995064841661576708271743181
        ) `shouldBe` "1b06164a2e86a170f0d8ac96cffa2e3312f2f5b0b1c3b1e082b9a0cd"

  it "Check BigNat serialization roundtrip for block #2 distance" $ do
    let b = BigNat . fromIntegral @Integer $ 2504075119295696010374311877171546620995064841661576708271743181
    (S.decode . S.encode) b == Right b

prop_BigNatSerializeRoundtrip :: BigNat -> Bool
prop_BigNatSerializeRoundtrip b =
    (S.decode . S.encode) b == Right b
