-- |
module Ergvein.Index.Server.TCPService.Supervisor
  ( -- * Linked worker threads
    withLinkedWorker
  , withLinkedWorker_
  , ExceptionInLinkedThread(..)
    -- * Worker unions
  , WorkersUnion
  , withWorkersUnion
  , spawnWorker
  ) where

import Control.Concurrent              (ThreadId,myThreadId,forkIO)
import Control.Concurrent.STM
import Control.Concurrent.Async.Lifted (Async,async)
import qualified Control.Concurrent.Async as Async
import Control.Exception               (Exception,AsyncException(..),throwIO,throwTo)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.IO.Unlift
import qualified Data.Set        as Set


-- | Unhandled exception in child thread.
data ExceptionInLinkedThread = ExceptionInLinkedThread SomeException
  deriving Show
instance Exception ExceptionInLinkedThread

-- | Create worker thread
withLinkedWorker
  :: (MonadBaseControl IO m)
  => m a -> (Async () -> m b) -> m b
withLinkedWorker action cont = restoreM =<< do
  -- FIXME: test that we correctly deal with blocking calls with
  --        throwTo. async use uninterruptibleCancel for example.
  liftBaseWith $ \runInIO -> do
    tid <- myThreadId
    mask $ \restore -> do
      -- Here we spawn worker thread which will throw unhandled exception to main thread.
      a <- Async.async $ restore (runInIO action) `catch` \e -> do
        unless (ignoreException e) $ throwTo tid (ExceptionInLinkedThread e)
        throwIO e
      restore (runInIO (cont (() <$ a))) `finally` Async.cancel a

-- | Same as 'withLinkedWorker' for use in cases when
withLinkedWorker_ :: (MonadBaseControl IO m) => m a -> m b -> m b
withLinkedWorker_ action = withLinkedWorker action . const

-- Exception to ignore for linked threads
ignoreException :: SomeException -> Bool
ignoreException e
  | Just Async.AsyncCancelled <- fromException e = True
  | Just ThreadKilled         <- fromException e = True
  | otherwise = False


-- | Set of worker threads
newtype WorkersUnion = WorkersUnion (TVar (Set.Set ThreadId))

-- | Create handler for set of worker threads which all will be
-- limited once 'withWorkersUnion' terminates.
withWorkersUnion
  :: (MonadIO m, MonadMask m)
  => (WorkersUnion -> m a) -> m a
withWorkersUnion = bracket ini fini
  where
    ini  = WorkersUnion <$> liftIO (newTVarIO mempty)
    fini (WorkersUnion tidsVar) = liftIO $ do
      tids <- readTVarIO tidsVar
      forM_ tids $ \tid -> forkIO $ throwTo tid Async.AsyncCancelled

-- | Spawn worker thread which will be terminated when
-- 'withWorkersUnion' exits.
spawnWorker
  :: (MonadBaseControl IO m, MonadMask m)
  => WorkersUnion -> m a -> m ()
spawnWorker (WorkersUnion tidsVar) action = do
  mask_ $ do
    a <- async $ action `finally` do
      liftBase (do tid <- myThreadId
                   atomically $ modifyTVar' tidsVar $ Set.delete tid)
    liftBase $ atomically $ modifyTVar' tidsVar $ Set.insert (Async.asyncThreadId a)
