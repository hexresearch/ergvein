module Main where

import System.Environment
import Crypto.Hash.MerkleTree

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show args
  pure ()