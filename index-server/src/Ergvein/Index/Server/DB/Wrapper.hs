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
  , reopenLevelDB
  , closeLevelDB
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
  { dbHandle   :: TVar DBState -- ^ Database handle.
  , dbActive   :: TVar Int     -- ^ Number of active DB operations
  , dbFilePath :: FilePath
  , dbOptions  :: Options
  }

data DBState
  = DbOpen   !DB  -- Database is open
  | DbClosed      -- Database is closed. Attempt to use it will throw
  | DbCloseLocked -- Database is closed and locked. Attempts to use it
                  -- will block. This state is used for reopening DB

-- | Open handle to a database
openLevelDB :: MonadIO m => FilePath -> Options -> m LevelDB
openLevelDB path opt = liftIO $ do
  dbActive <- newTVarIO 0
  dbHandle <- newTVarIO . DbOpen =<< open path opt
  pure LevelDB{ dbFilePath = path
              , dbOptions  = opt
              , ..
              }

-- | Close and then open database again.
reopenLevelDB :: MonadIO m => LevelDB -> m ()
reopenLevelDB LevelDB{..} = liftIO $ do
  mdb <- atomically $ readTVar dbHandle <* writeTVar dbHandle DbCloseLocked
  closeHandle dbActive mdb
  -- reopen database
  atomically . writeTVar dbHandle . DbOpen =<< open dbFilePath dbOptions

-- | Close handle. After close it's no longer usable and any database
-- operation which uses this handle will throw exception.
closeLevelDB :: MonadIO m => LevelDB -> m ()
-- FIXME: Masking???
closeLevelDB LevelDB{..} = liftIO $ do
  -- First mark database as closed so no new transaction could be started
  mdb <- atomically $ readTVar dbHandle <* writeTVar dbHandle DbClosed
  closeHandle dbActive mdb

closeHandle :: TVar Int -> DBState -> IO ()
closeHandle active = \case
    DbClosed      -> pure ()
    DbCloseLocked -> pure ()
    DbOpen   db   -> do
      -- Wait until all requests are finished then close database
      atomically $ check . (==0) =<< readTVar active
      unsafeClose db

usingLevelDB :: MonadIO m => LevelDB -> (DB -> IO a) -> m a
usingLevelDB LevelDB{..} = liftIO . bracket ini fini
  where
    ini = atomically $ readTVar dbHandle >>= \case
      DbOpen   db   -> db <$ modifyTVar' dbActive succ
      DbClosed      -> error "LevelDB: Database is closed"
      DbCloseLocked -> retry
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
