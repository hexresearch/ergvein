{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Options.Applicative
import Scan
import Data.Text
import Data.Semigroup ((<>))
import Data.FileEmbed
import qualified Data.ByteString as BS
import Data.MerkleTree
import Data.ByteString.Char8 as C8



scanConfig :: Parser ScanConfig
scanConfig = ScanConfig
      <$> strOption ( long "host")
      <*> option auto ( long "port")
      <*> strOption ( long "user")
      <*> strOption ( long "password")
      <*> option auto ( long "chunkSize")
      <*> strOption ( long "fileName")

main :: IO ()
main = do
  args <- getArgs
  scanConfig <- execParser opts
  scanToFile scanConfig
  where
    opts = info (scanConfig <**> helper) fullDesc


