module Main where

#ifdef ANDROID
import Sepulcas.Android.Run()
import Sepulcas.Android.Native()
#else
import Sepulcas.Desktop.Run()
import Sepulcas.Desktop.Native()
#endif

main :: IO ()
main = pure ()
