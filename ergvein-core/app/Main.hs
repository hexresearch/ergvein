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
import Reflex.Fork
import Reflex.PerformEvent.Base

import Data.IORef
import Data.Dependent.Sum (DSum (..))
import Control.Monad.Ref
import Data.Functor.Identity
import qualified Control.Monad.Fail as F
import qualified Reflex.Profiled as RP

type EventChannel = Chan [DSum (EventTriggerRef Spider) TriggerInvocation]

main :: IO ()
main = (runSpiderHost :: SpiderHost Global a -> IO a) $ do
  events <- liftIO newChan
  ((result, postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    result <- runPostBuildT (runTriggerEventT (f undefined undefined undefined) events) postBuild
    pure (result, postBuildTriggerRef)
  mPostBuildTrigger <- readRef postBuildTriggerRef
  forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
  liftIO $ processAsyncEvents events fc

processAsyncEvents :: EventChannel -> FireCommand Spider (SpiderHost Global) -> IO ()
processAsyncEvents events (FireCommand fire) = void $ forkIO $ forever $ do
  ers <- readChan events
  _ <- runSpiderHost $ do
    mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
      me <- readIORef er
      return $ fmap (\e -> e :=> Identity a) me
    _ <- fire (catMaybes mes) $ return ()
    liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  return ()

instance F.MonadFail (SpiderHostFrame Global) where
  fail = liftIO . F.fail

instance F.MonadFail (RP.ProfiledM (SpiderHostFrame Global)) where
  fail = liftIO . F.fail
