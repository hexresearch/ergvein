module Scan where

import qualified Network.Bitcoin.Api.Client as BitcoinApi
import Network.Bitcoin.Api.Blockchain
import Data.Text (Text, pack)
import Data.HexString
import Data.Maybe
import Data.List.Split
import Crypto.Hash
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.MerkleTree
import qualified Data.Text.IO as TIO

scan :: String -> Int -> Text -> Text -> IO ()
scan host port user password = do
  count <- client getBlockCount
  r <- mapM itera $ [0 .. 9999]
  let sp = mkMerkleTree $ convert . hashFinalize . hashUpdates (hashInitWith SHA256) <$> chunksOf 1000 r
  TIO.writeFile "out" $ pack $ show sp
  pure ()
  where
    client = BitcoinApi.withClient host port user password
    itera x = do
      putStrLn $ "getting at " <> show x
      hash <-  client $ flip getBlockHash x 
      r <- client $ flip getBlockHeader hash
      pure $ toBytes $ fromJust r 
      