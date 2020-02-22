{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.Cache.Monad where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Default
import Data.Either
import Data.Flat
import Data.Maybe
import Data.Word8

import qualified Database.LevelDB as LDB
import qualified Data.ByteString as BS

class  MonadUnliftIO m => MonadLDB m where
  getDb :: m LDB.DB

data AreaKey = AreaKey 
  { keyPrefix :: BS.ByteString
  , keyBase   :: BS.ByteString
  } deriving (Generic, Flat)

keyString :: (Flat k) => Word8 -> k -> BS.ByteString
keyString keyPrefix key = keyPrefix `BS.cons` flat key

putItems :: (Flat v) => (a -> BS.ByteString) -> (a -> v) -> [a] -> LDB.WriteBatch
putItems keySelector valueSelector items = putI <$> items
  where putI item = LDB.Put (keySelector item) $ flat $ valueSelector item

unflatExact :: (Flat b) => BS.ByteString -> b
unflatExact s = case unflat s of
    Right k -> k
    Left e -> error $ show e ++ "value " ++  show s