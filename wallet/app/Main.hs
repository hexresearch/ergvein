module Main where

import Data.Default
import Ergvein.Wallet
import Ergvein.Wallet.Style
import Ergvein.Wallet.Yaml
import GHC.Generics
import Options.Generic
import Reflex.Dom

data Options = Options {
  config :: Maybe FilePath <?> "Path to config file"
} deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
  opts <- getRecord "Furtovina launcher"
  settings :: Settings <- maybe (pure def) readYaml' $ unHelpful $ config opts
  env <- newEnv settings
  mainWidgetWithCss frontendCssBS $ runEnv env frontend
