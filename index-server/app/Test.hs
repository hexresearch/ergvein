{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Word
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.String
import Data.Maybe
import Data.Either
import Database.SQLite.Simple
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Text.InterpolatedString.Perl6 (qc)
import Data.List as List

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Utxo
import Ergvein.Types
import Ergvein.Text
import Ergvein.Types.Transaction
import qualified Data.Serialize          as S

main :: IO ()
main = do
  -- let th = BtcTxHash $ "0437cd7f8525ceed2324359c2d0ba26006d92d856a9c20fa0241106ee5a597c9"
  -- print th
  -- let a = S.decode @TxHash $ S.encode th
  -- print $ a
  -- print $ (th ==) <$> a
  conn <- open "db/lol.db"
  execute_ conn [qc|
    create table if not exists lol (
      id int primary key,
      val int not null);
  |]

  executeMany conn [qc|
    insert or replace into lol (id, val) values (?,?)
  |] ([(1,1), (2,1), (3,1), (4,5)] :: [(Int, Int)])

  vals :: [(Int, Int)] <- query_ conn [qc|select * from lol|]
  forM vals print


  -- let dels :: [Only Int] = [Only 1, Only 2]
  let dels :: [Only Int] = []
  executeMany conn [qc| delete from lol where id = ? |] dels

  vals' :: [(Int, Int)] <- query_ conn [qc|select * from lol|]
  forM vals' print

  putStrLn "Hello World"
