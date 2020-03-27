{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.Cache.Monad where

import Control.Monad.IO.Unlift
import qualified Database.LevelDB as LDB

class  MonadIO m => MonadLDB m where
  getDb :: m LDB.DB