{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Ergo.Modifier
import Data.Ergo.Protocol
import Data.Ergo.Protocol.Client
import Data.Ergo.FFI
import Data.Maybe
import Data.Time
import Options.Generic
import Reflex
import Reflex.Host.Class
import Reflex.Spider.Orphans ()
import Ergvein.Core.Node.Ergo
import Control.Monad.IO.Unlift
import Reflex.Spider.Internal




main :: IO ()
main = (runSpiderHost :: SpiderHost Global a -> IO a) $ do
  f undefined undefined undefined
  pure ()
 