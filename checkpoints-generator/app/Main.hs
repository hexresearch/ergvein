module Main where

import System.Environment
import Crypto.Hash.MerkleTree
import Options.Applicative
import Data.Semigroup ((<>))

data BTCNodeConnection = BTCNodeConnection
  { host     :: String
  , port     :: Int
  , user     :: String
  , password :: String
  } deriving Show

btcNodeConnection :: Parser BTCNodeConnection
btcNodeConnection = BTCNodeConnection
      <$> strOption ( long "host")
      <*> option auto ( long "port")
      <*> strOption ( long "user")
      <*> strOption ( long "password")

main :: IO ()
main = do
  args <- getArgs
  nodeConnection <- execParser opts
  putStrLn $ show nodeConnection
  pure ()
  where
    opts = info (btcNodeConnection <**> helper) fullDesc
