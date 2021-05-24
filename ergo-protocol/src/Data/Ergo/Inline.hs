{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Data.Ergo.Inline(
    testFunc
  ) where

import Language.Rust.Inline
import Language.Haskell.TH
import Data.Int

extendContext basic
setCrateRoot []

testFunc :: IO ()
testFunc = do
  putStrLn "Haskell: Hello. Enter a number:"
  x <- readLn
  y <- [rustIO| i32 {
    println!("Rust: Your number is {}", $(x: i32));
    $(x: i32) + 1
  } |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y
