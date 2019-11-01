{-# LANGUAGE CPP #-}
module Main where

import Data.Default
import Ergvein.Wallet
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Ergvein.Wallet.Style
import Ergvein.Wallet.Yaml
import GHC.Generics
import Options.Generic

#ifdef ANDROID
import Ergvein.Wallet.Android.Run
import Ergvein.Wallet.Android.Native
#else
import Ergvein.Wallet.Desktop.Run
import Ergvein.Wallet.Desktop.Native
#endif

data Options = Options {
  config :: Maybe FilePath <?> "Path to config file"
} deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
  opts <- getRecord "Ergvein cryptowallet"
  settings :: Settings <- getSettings $ unHelpful $ config opts
  run $ \cbs -> do
    css <- compileFrontendCss
    mainWidgetWithCss css $ do
      env <- newEnv settings (runUiCallbacks cbs)
      runEnv cbs env frontend
