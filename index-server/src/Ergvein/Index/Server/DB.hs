{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.DB
  (
    DBTag(..)
  ) where

import Control.Monad.Catch

import Ergvein.Index.Server.DB.Monad

data MyException = DbVersionMismatch
    deriving Show

instance Exception MyException
