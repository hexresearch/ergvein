{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Default
import Database.RocksDB

import Data.Serialize as S

import Control.Monad.IO.Class

main :: IO ()
main = do
  let a :: IO () = print "a"
  unLolM $ LolM $ foo a

  -- putStrLn "Hello World"
  -- let cfg = def {createIfMissing = True}
  -- withDBCF "db" cfg [("One",def),("Two", def)]$ \db -> do
  --   let [one, two] = columnFamilies db
  --   let k = S.encode @Int 1
  --   let v1 = S.encode @Int 1
  --   let v2 = S.encode @Int 2
  --
  --   putCF db one k v1
  --   putCF db one k v2
  --   v <- getCF db one k
  --   print $ (S.decode @Int) <$> v
  --   v' <- getCF db two k
  --   print $ (S.decode @Int) <$> v'
  --   pure ()

class MonadIO m => LolMonad m where
  lolwut :: m ()

foo :: LolMonad m => (forall n . MonadIO n => n ()) -> m ()
foo m = m

newtype LolM a = LolM {unLolM :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance LolMonad LolM where
  lolwut = pure ()
