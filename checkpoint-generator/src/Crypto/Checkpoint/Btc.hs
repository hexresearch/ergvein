module Crypto.Checkpoint.Btc where

import Crypto.Checkpoint.Utils
import Data.ByteString (ByteString)
import Data.Conduit
import Data.HexString
import Data.Maybe
import Data.MerkleTree
import Data.Text (Text, pack)
import Network.Bitcoin.Api.Blockchain
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as TIO
import qualified Network.Bitcoin.Api.Client as BitcoinApi

data ScanConfig = ScanConfig
  { cfgNodeHost     :: String
  , cfgNodePort     :: Int
  , cfgNodeUser     :: Text
  , cfgNodePassword :: Text
  , cfgChunkSize    :: Integer
  , cfgFileName     :: String
  } deriving Show

scanToFile :: ScanConfig -> IO ()
scanToFile scanConfig= do
  headersMerkleTree <- headersMerkleTree scanConfig
  TIO.writeFile (cfgFileName scanConfig) $ pack $ show headersMerkleTree

headersMerkleTree :: ScanConfig -> IO (MerkleTree ByteString)
headersMerkleTree scanConfig = do
  nodeHeight <- client getBlockCount

  let roundNodeHeight = nodeHeight - nodeHeight `rem` cfgChunkSize scanConfig

  tree <- runConduit $ 
    CL.enumFromTo 0 roundNodeHeight
    .| CL.mapM (blockHeaderAtHeight roundNodeHeight)
    .| CL.chunksOf (fromIntegral $ cfgChunkSize scanConfig)
    .| CL.map chunkHash
    .| CC.sinkList
         
  pure $ mkMerkleTree tree
  where
    client = BitcoinApi.withClient 
      (cfgNodeHost scanConfig)
      (cfgNodePort scanConfig)
      (cfgNodeUser scanConfig)
      (cfgNodePassword scanConfig)

    blockHeaderAtHeight :: Integer -> Integer -> IO HexString
    blockHeaderAtHeight nodeHeight heightToScan = do
      putStrLn $ "scanning at " <> show heightToScan <> " " <> show (succ heightToScan * 100 `div` nodeHeight)
      blockHash <-  client $ flip getBlockHash heightToScan
      blockHeader <- client $ flip getBlockHeader blockHash
      pure $ fromJust blockHeader