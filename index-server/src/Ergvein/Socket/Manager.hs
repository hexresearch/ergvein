module Ergvein.Socket.Manager
  (
    HasSocketsManagement(..)
  , HasThreadsManagement(..)
  , killRecursively
  , forkManaged
  , closeSocketAndKillThreads
  , closeAllSockets
  ) where

import Control.Concurrent
import Control.Monad.Trans.Control
import Control.Concurrent.Lifted (fork)
import Control.Monad.IO.Unlift
import Network.Socket

class HasThreadsManagement m where
  registerThread :: ThreadId -> ThreadId -> m ()
  getThreadChildren :: ThreadId -> m [ThreadId]

killRecursively :: (HasThreadsManagement m, MonadIO m) => ThreadId -> m ()
killRecursively tid = do
  tids <- getThreadChildren tid
  mapM_ killRecursively tids
  liftIO $ killThread tid

forkManaged :: (HasThreadsManagement m, MonadBaseControl IO m, MonadIO m) => m () -> m ThreadId
forkManaged m = do
  myTid <- liftIO myThreadId
  childTid <- fork m
  registerThread myTid childTid
  pure childTid

class HasSocketsManagement m where
  registerSocket :: SockAddr -> Socket -> ThreadId -> m ()
  deregisterSocket :: SockAddr -> m ()
  getManagedSocket :: SockAddr -> m (Maybe (Socket, ThreadId))
  getAllManagedSockets :: m [SockAddr]

closeSocketAndKillThreads :: (
    HasSocketsManagement m
  , HasThreadsManagement m
  , MonadIO m) => SockAddr -> m ()
closeSocketAndKillThreads addr = do
  mstid <- getManagedSocket addr
  case mstid of
    Nothing -> pure ()
    Just (sock, tid) -> do
      liftIO $ close sock
      killRecursively tid

closeAllSockets :: (HasSocketsManagement m, HasThreadsManagement m, MonadIO m) => m ()
closeAllSockets = mapM_ closeSocketAndKillThreads =<< getAllManagedSockets
