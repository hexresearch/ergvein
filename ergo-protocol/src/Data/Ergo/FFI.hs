module Data.Ergo.FFI(
    testFunc
  ) where

import Data.Int

foreign import ccall safe "test_ffi_func" test_ffi_func
      :: Int32 -> IO Int32

testFunc :: IO ()
testFunc = do
  putStrLn "Haskell: Hello. Enter a number:"
  x <- readLn
  y <- test_ffi_func x
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y
