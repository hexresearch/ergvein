module Ergvein.Wallet.Yaml(
    loadYaml
  , loadYamlEither
  , readYaml
  , readYaml'
  , readYamlEither'
) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString as BS       (readFile)
import Data.Yaml                   (decodeEither')
import Data.Yaml.Config            (loadYamlSettings, useEnv)
import qualified Control.Exception   as Exception
import qualified Data.Text           as T

loadYaml ::  FromJSON settings => ByteString -> IO settings
loadYaml bs = loadYamlSettings [] [value] useEnv
  where
    value = either Exception.throw id $ decodeEither' bs

loadYamlEither ::  FromJSON settings => ByteString -> IO (Either T.Text settings)
loadYamlEither bs = Exception.catch (fmap Right $ loadYaml bs)
  (\(ex :: Exception.SomeException) -> pure $ Left $ T.pack $ show ex)

readYaml' :: FromJSON settings => FilePath -> IO settings
readYaml' fp = loadYaml =<< BS.readFile fp

readYamlEither' :: FromJSON settings => FilePath -> IO (Either T.Text settings)
readYamlEither' fp = loadYamlEither =<< BS.readFile fp

readYaml :: FromJSON settings => FilePath -> IO settings
readYaml fp = loadYamlSettings [fp] [] useEnv
