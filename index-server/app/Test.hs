{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Default
import Database.RocksDB

import Data.Serialize as S

import Control.Monad.IO.Class

mkChunks :: Int -> [a] -> [[a]]
mkChunks n vals = mkChunks' [] vals
  where
     mkChunks' acc xs = case xs of
       [] -> acc
       _ -> let (a,b) = splitAt n xs in mkChunks' (acc ++ [a]) b

mkEquisizedChunks :: Int -> [a] -> [[a]]
mkEquisizedChunks n vals = let
  l = length vals
  (q,r) = quotRem l n
  (v1,v2) = splitAt (q + r) vals
  in [v1] ++ mkChunks q v2

main :: IO ()
main = do
  print $ fmap length $ mkEquisizedChunks 3 [1..15]
