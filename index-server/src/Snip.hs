{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
module Main where

import Data.Word
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.Async
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.String
import System.Random.Mersenne as R
import Data.Maybe
import Data.Default
import Database.SQLite.Simple
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Text.InterpolatedString.Perl6 (qc)
import Data.List as List

fetch r conn  = do

  forever $ do
    n <- R.random r :: IO Int
    let zu = fromString (show n)
    k <- query conn "select val from test where id = ?" (Only zu :: Only ByteString) :: IO [Only ByteString]
    pure ()
  threadDelay 300000
--   print (n, zu)

gen conn = do
  st  <- openStatement conn "insert into test (id,val) values (?,?)"
  rs <- forM_ [1..1000] $ \i -> do
          let z = [ fromString (show (i*10000+k)) | k <- [1..10000] ] :: [ByteString]
          execute_ conn "begin"
          execute_ conn "delete from test"
          forM_ z $ \k -> do
            reset st
            bind st (k,k)
            _ <- nextRow st :: IO (Maybe (Only Int))
            pure ()
--           execute_ conn [qc|create table disk.test{i} (id blob primary key, val blob)|]
--           execute_ conn [qc|insert into disk.test{i} (id,val) select id,val from test|]
          execute_ conn [qc|insert into disk.test (id,val) select id,val from test|]
          execute_ conn "commit"
          pure ()

  closeStatement st
--   n <- query_ conn "select count(1) from test" :: IO [Only Int]
--   print n


ggen :: Int -> String -> Connection -> (Int -> Bool) -> [Int] -> IO()
ggen n p conn flt lss = do
  st  <- openStatement conn [qc|insert into {p} (id,val) values (?,?)|]

  loop st lss

  closeStatement st

  where
    loop _ [] = pure ()
    loop st ls = do
      let (xs,rs) = splitAt (n*10000) ls
      let it = filter flt xs
      execute_ conn "begin"
      execute_ conn "delete from test"
      forM_ it $ \k -> do
        reset st
        bind st (k,k)
        _ <- nextRow st :: IO (Maybe (Only Int))
        pure ()
      execute_ conn [qc|insert into disk.{p} (id,val) select id,val from test|]
      execute_ conn "commit"
      loop st rs

makeConn :: String -> IO (Connection,Connection)
makeConn s = do
  conn <- open ":memory:"
  execute_ conn "CREATE TABLE IF NOT EXISTS test (id BLOB, val BLOB)"
  execute_ conn "PRAGMA synchronous=OFF;"
  execute_ conn "pragma journal_mode = WAL;"
  conn2 <- open s
  execute_ conn2 "CREATE TABLE IF NOT EXISTS test (id BLOB PRIMARY KEY, val BLOB)"
  execute_ conn2 "PRAGMA synchronous=OFF;"
  execute_ conn2 "pragma journal_mode = WAL;"
  execute_ conn  [qc|attach '{s}' as disk|]
  pure (conn,conn2)

modn k n x = x `mod` k == n

main :: IO ()
main = do
  r <- newMTGen Nothing

  (conn11, conn12)  <- makeConn "db/db0"
  (conn21, conn22)  <- makeConn "db/db1"
  (conn31, conn32)  <- makeConn "db/db2"
  (conn41, conn42)  <- makeConn "db/db3"

  let lst = [1..100000000]

  a1 <- async $ ggen 4 "test" conn11 (modn 4 0) lst
  a2 <- async $ ggen 4 "test" conn21 (modn 4 1) lst
  a3 <- async $ ggen 4 "test" conn31 (modn 4 2) lst
  a4 <- async $ ggen 4 "test" conn41 (modn 4 3) lst
  an <- async $ pure () -- fetch r conn2
  mapM_ wait [a1,a2,a3,a4]
--   mapM_ wait [a1,a2] --,a3,a4]
  cancel an

  mapM_ close [conn11, conn12, conn21, conn22, conn31, conn32, conn41, conn42]
--   mapM_ close [conn11, conn12] --, conn21, conn22, conn31, conn32, conn41, conn42]

--   execute_ conn "insert into disk.test (id,val) select id,val from test"

--   a1 <- async $ pure () -- fetch r conn
--   a1 <- async $ gen1 conn2
--   a2 <- async $ gen2 conn2
--   a3 <- async $ gen3 conn2
--   a2 <- async $ gen3 conn2
--   a3 <- async $ writer conn conn2

--   mapM_ wait [a1]
--   mapM_ wait [a1,a2,a3]
--   wait a2
--   cancel a1

  putStrLn "done"

--}
