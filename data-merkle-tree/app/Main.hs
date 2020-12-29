module Main where

import Criterion.Main
import Data.MerkleTree
import qualified Data.Serialize as S

main :: IO ()
main = defaultMain
  [ bench "constructing tree"  $ nf mkMerkleTree leafData
  , bench "constructing leaf hash"  $ nf mkLeafRootHash leaf
  , bench "constructing leaf proof"  $ nf (uncurry merkleProof) (tree, leafHash)
  , bench "proof validation"  $ nf (\(proof, root, hash)-> validateMerkleProof proof root hash) (leafProof, (mtRoot tree), leafHash)
  ]
  where
    leaf = S.encode (512::Int)
    leafData = S.encode <$> [(1::Int)..1024]
    tree = mkMerkleTree leafData
    leafHash = mkLeafRootHash leaf
    leafProof = merkleProof tree leafHash
