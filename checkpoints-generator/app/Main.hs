module Main where

import System.Environment
import Crypto.Hash.MerkleTree
import Options.Applicative
import Scan
import Data.Text
import Data.Semigroup ((<>))

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

main :: IO ()
main = do
  args <- getArgs
  nodeConnection <- execParser opts
  scan (host nodeConnection) (port nodeConnection) (user nodeConnection) (password nodeConnection)
  pure ()
  where
    opts = info (btcNodeConnection <**> helper) fullDesc
