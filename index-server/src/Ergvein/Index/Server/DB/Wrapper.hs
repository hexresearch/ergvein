-- |
-- This module provides wrapper for LevelDB's connection which allows
-- safe closing in concurrent setting. Any use of connection after
-- 'unsafeClose' will lead memory corruption. (It's called unsafe for
-- reason). When connection is shared by several threads it's
-- 'withConnection' is not safe either. Thread may be killed _after_
-- connection is closed.
module Ergvein.Index.Server.DB.Wrapper
  ( LevelDB
  , openLevelDB
  , closeLevelDB
  , moveLevelDbHandle
    -- * Wrapped operations
  , writeLDB
  , getLDB
  , putLDB
  , streamSliceLDB
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import Data.Default
import Data.ByteString           (ByteString)
import Database.LevelDB.Base
import Database.LevelDB.Internal (unsafeClose)
import Database.LevelDB.Streaming (entrySlice,Entry,KeyRange,Direction)
import qualified Database.LevelDB.Streaming as Stream

-- | Connection to database. Actual connection is stored inside @MVar@
-- and @Nothing@ is stored on when connection is closed. This way user
-- won't be able to use now invalid connection,
data LevelDB = LevelDB
  { dbHandle :: TVar (Maybe DB) -- ^ Database handle.
  , dbActive :: TVar Int        -- ^ Number of active DB operations
  }

-- | Open handle to a database
openLevelDB :: MonadIO m => FilePath -> Options -> m LevelDB
openLevelDB path opt = liftIO $ do
  dbActive <- newTVarIO 0
  dbHandle <- newTVarIO . Just =<< open path opt
  pure LevelDB{..}

-- | Close and then immediately reopen database
moveLevelDbHandle :: MonadIO m => LevelDB -> LevelDB -> m ()
moveLevelDbHandle src dst = liftIO $ do
  -- First close destination database
  closeLevelDB dst
  -- Move new one.
  atomically $ do
    writeTVar (dbHandle dst) =<< readTVar (dbHandle src)
    writeTVar (dbActive dst) =<< readTVar (dbActive src)
    writeTVar (dbHandle src) Nothing
    writeTVar (dbActive src) 0

-- | Close handle. After close it's no longer usable and any database
-- operation which uses this handle will throw exception.
closeLevelDB :: MonadIO m => LevelDB -> m ()
-- FIXME: Masking???
closeLevelDB LevelDB{..} = liftIO $ do
  -- First mark database as closed so no new transaction could be started
  mdb <- atomically $ readTVar dbHandle <* writeTVar dbHandle Nothing
  case mdb of
    Nothing -> pure ()
    Just db -> do
      -- Wait until all requests are finished then close database
      atomically $ check . (==0) =<< readTVar dbActive
      unsafeClose db


usingLevelDB :: MonadIO m => LevelDB -> (DB -> IO a) -> m a
usingLevelDB LevelDB{..} = liftIO . bracket ini fini
  where
    ini = atomically $ readTVar dbHandle >>= \case
      Nothing -> error "LevelDB: Database is closed"
      Just db -> db <$ modifyTVar' dbActive succ
    fini _ = atomically $ modifyTVar' dbActive pred


----------------------------------------------------------------
-- Wrappers
----------------------------------------------------------------

writeLDB :: MonadIO m => LevelDB -> WriteOptions -> WriteBatch -> m ()
writeLDB db opt batch = usingLevelDB db $ \c -> write c opt batch

getLDB :: MonadIO m => LevelDB -> ReadOptions -> ByteString -> m (Maybe ByteString)
getLDB db opt key = usingLevelDB db $ \c -> get c opt key

putLDB :: MonadIO m => LevelDB -> WriteOptions -> ByteString -> ByteString -> m ()
putLDB db opt k v = usingLevelDB db $ \c -> put c opt k v

-- | Stripped down but safe variant of streaming data from levelDB. It
-- fetches all data in one go
streamSliceLDB :: MonadIO m => LevelDB -> KeyRange -> Direction -> m [Entry]
streamSliceLDB db range dir
  = usingLevelDB db
  $ \c    -> bracket (createIter c def) releaseIter
  $ \iter -> Stream.toList $ entrySlice iter range dir
