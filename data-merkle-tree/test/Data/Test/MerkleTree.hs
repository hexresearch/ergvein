module Data.Test.MerkleTree where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances ()
import qualified Data.ByteString as BS

import Data.MerkleTree

test_tests :: TestTree
test_tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "tree gives positive proof if source contains element" positiveProof
  , QC.testProperty "tree gives negative proof if source does not contains element" negativeProof
  ]

positiveProof :: BS.ByteString -> NonEmptyList BS.ByteString -> Property
positiveProof smpl nonEmptyList = sourceNotContainsSample ==> merkleProofIsPositive
  where
    source = getNonEmpty nonEmptyList
    sourceNotContainsSample = not $ elem smpl source
    merkleProofIsPositive = let 
      tree = mkMerkleTree $ smpl : source
      leafHash = mkLeafRootHash smpl
      leafProof = merkleProof tree leafHash
      in validateMerkleProof leafProof (mtRoot tree) leafHash
negativeProof :: BS.ByteString -> NonEmptyList BS.ByteString -> Property
negativeProof smpl nonEmptyList = sourceNotContainsSample ==> merkleProofIsNegative
  where
    source = getNonEmpty nonEmptyList
    sourceNotContainsSample = not $ elem smpl source
    merkleProofIsNegative = let 
      tree = mkMerkleTree source
      leafHash = mkLeafRootHash smpl
      leafProof = merkleProof tree leafHash
      in not $ validateMerkleProof leafProof (mtRoot tree) leafHash