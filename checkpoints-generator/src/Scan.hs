module Scan where

import qualified Network.Bitcoin.Api.Client as BitcoinApi
import Network.Bitcoin.Api.Blockchain
import Data.Text (Text)
import Data.HexString
import Data.Maybe
import Data.List.Split
import Crypto.Hash
import Data.ByteArray
import Data.ByteString (ByteString)

scan :: String -> Int -> Text -> Text -> IO ()
scan host port user password = do
  count <- client getBlockCount
  let f = hashInitWith SHA256
  r <- mapM itera $ [0 .. 99 ]
  let 
    sp :: [ByteString]
    sp = convert . hashFinalize . hashUpdates f <$> chunksOf 10 r
  putStrLn $ show sp
  pure ()
  where
    client = BitcoinApi.withClient host port user password
    itera x = do
      putStrLn $ "getting at " <> show x
      hash <-  client $ flip getBlockHash x 
      r <- client $ flip getBlockHeader hash
      pure $ toBytes $ fromJust r 
      