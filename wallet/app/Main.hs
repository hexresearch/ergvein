{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Data.Text (unpack)
import Ergvein.Wallet
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Ergvein.Wallet.Style
import Ergvein.Wallet.Version
import GHC.Generics
import Options.Generic

#ifdef ANDROID
import Ergvein.Wallet.Android.Run()
import Ergvein.Wallet.Android.Native()
#else
import Ergvein.Wallet.Desktop.Run()
import Ergvein.Wallet.Desktop.Native()
#endif

data Options = Options {
  config :: Maybe FilePath <?> "Path to config file"
} deriving (Generic)

instance ParseRecord Options

instance HasVersion where
  version = $embedVersion
  {-# NOINLINE version #-}

main :: IO ()
main = do
  putStrLn $ "Ergvein version: " <> unpack (makeVersionString version)
  opts <- getRecord "Ergvein cryptowallet"
  bindSelf $ run $ \cbs -> do
    css <- compileFrontendCss
    mainWidgetWithCss css $ do
      settings :: Settings <- loadSettings $ unHelpful $ config opts
      env <- newEnv settings (runUiCallbacks cbs)
      runEnv cbs env frontend
