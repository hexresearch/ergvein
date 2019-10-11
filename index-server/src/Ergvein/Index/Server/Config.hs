module Ergvein.Index.Server.Config where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.ByteString
import Data.Yaml.Config
import Ergvein.Aeson
import GHC.Generics

data Config = Config 
  { configDb :: !String
  } deriving (Show, Generic)
deriveJSON (aesonOptionsStripPrefix "config") ''Config

loadConfig :: MonadIO m => FilePath -> m Config
loadConfig path = liftIO $ loadYamlSettings [path] [] useEnv