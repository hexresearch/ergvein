{-# LANGUAGE CPP #-}
module Main where

import Data.Default
import Ergvein.Wallet
import Ergvein.Wallet.Run
import Ergvein.Wallet.Style
import Ergvein.Wallet.Yaml
import GHC.Generics
import Options.Generic

#ifdef ANDROID
import Ergvein.Wallet.Android.Run
#else
import Ergvein.Wallet.Desktop.Run
#endif

data Options = Options {
  config :: Maybe FilePath <?> "Path to config file"
} deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
  opts <- getRecord "Ergvein cryptowallet"
  settings :: Settings <- maybe (pure def) readYaml' $ unHelpful $ config opts
  run $ \cbs -> do
    css <- compileFrontendCss
    mainWidgetWithCss css $ do
      env <- newEnv settings
      runEnv cbs env frontend
