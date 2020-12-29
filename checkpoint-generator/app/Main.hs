{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Crypto.Checkpoint.Btc

scanConfig :: Parser ScanConfig
scanConfig = ScanConfig
      <$> strOption   ( long "host")
      <*> option auto ( long "port")
      <*> strOption   ( long "user")
      <*> strOption   ( long "password")
      <*> option auto ( long "chunkSize")
      <*> strOption   ( long "fileName")

main :: IO ()
main = scanToFile =<< execParser opts
  where
    opts = info (scanConfig <**> helper) fullDesc
