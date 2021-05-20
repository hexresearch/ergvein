{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Sepulcas

#ifdef ANDROID
import Sepulcas.Android.Run()
import Sepulcas.Android.Native()
#else
import Sepulcas.Desktop.Run()
import Sepulcas.Desktop.Native()
#endif

-- | Languages that are supported by wallet
data instance Language = English
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance LocalizedPrint Language where
  localizedShow _ v = case v of
    English -> "English"

main :: IO ()
main = bindSelf $ run $ \cbs -> do
  css <- compileStyles $ pure ()
  mainWidgetWithCss css $ do
    env <- newSepulca Nothing English (runUiCallbacks cbs)
    runSepulca cbs env frontend

frontend :: Sepulcable t m => m ()
frontend = pure ()
