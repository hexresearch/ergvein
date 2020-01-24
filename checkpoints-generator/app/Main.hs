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

data BTCNodeConnection = BTCNodeConnection
  { host     :: String
  , port     :: Int
  , user     :: Text
  , password :: Text
  } deriving Show

btcNodeConnection :: Parser BTCNodeConnection
btcNodeConnection = BTCNodeConnection
      <$> strOption ( long "host")
      <*> option auto ( long "port")
      <*> strOption ( long "user")
      <*> strOption ( long "password")

tree :: MerkleTree BS.ByteString
tree = read $ $(embedStringFile "out")

main :: IO ()
main = do
  args <- getArgs
  nodeConnection <- execParser opts
  let leafHash = mkLeafRootHash g
      leafProof = merkleProof tree leafHash
  error $ show $ validateMerkleProof leafProof (mtRoot tree) leafHash
  --scan (host nodeConnection) (port nodeConnection) (user nodeConnection) (password nodeConnection)
  pure ()
  where
    opts = info (btcNodeConnection <**> helper) fullDesc


