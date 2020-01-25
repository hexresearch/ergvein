module Utils where

import Crypto.Hash
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.HexString
import Data.MerkleTree

chunkBorders :: Integer -> Integer -> (Integer, Integer)
chunkBorders chunkSize x = let 
    from = x - x `rem` chunkSize
    in (x, from + chunkSize)

chunkHash :: [HexString] -> ByteString
chunkHash = convert . hashFinalize . hashUpdates (hashInitWith SHA256) . map toBytes

headersChunkPresence :: MerkleTree ByteString -> [HexString] -> Bool
headersChunkPresence tree chunk = let
  leafHash = mkLeafRootHash $ chunkHash chunk
  leafProof = merkleProof tree leafHash
  in validateMerkleProof leafProof (mtRoot tree) leafHash