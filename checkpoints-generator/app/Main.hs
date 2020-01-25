{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Options.Applicative
import Scanning
import Data.Semigroup ((<>))
import Data.ByteString.Char8 as C8

scanConfig :: Parser ScanConfig
scanConfig = ScanConfig
      <$> strOption   ( long "host")
      <*> option auto ( long "port")
      <*> strOption   ( long "user")
      <*> strOption   ( long "password")
      <*> option auto ( long "chunkSize")
      <*> strOption   ( long "fileName")

main :: IO ()
main = do
  args <- getArgs
  scanConfig <- execParser opts
  scanToFile scanConfig
  where
    opts = info (scanConfig <**> helper) fullDesc