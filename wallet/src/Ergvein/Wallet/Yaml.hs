module Ergvein.Wallet.Yaml(
    loadYaml
  , readYaml
  , readYaml'
) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString as BS       (readFile)
import Data.Yaml                   (decodeEither')
import Data.Yaml.Config            (loadYamlSettings, useEnv)
import qualified Control.Exception   as Exception

loadYaml ::  FromJSON settings => ByteString -> IO settings
loadYaml bs = loadYamlSettings [] [value] useEnv
  where
    value = either Exception.throw id $ decodeEither' bs

readYaml' :: FromJSON settings => FilePath -> IO settings
readYaml' fp = loadYaml =<< BS.readFile fp

readYaml :: FromJSON settings => FilePath -> IO settings
readYaml fp = loadYamlSettings [fp] [] useEnv
