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

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class
import Data.Default
import Data.ByteString           (ByteString)
import qualified Data.Serialize as S
import Database.LevelDB.Base
import Database.LevelDB.Internal (unsafeClose)
import Database.LevelDB.Streaming (entrySlice,Entry,KeyRange,Direction)
import qualified Database.LevelDB.Streaming as Stream

-- | Connection to database. Actual connection is stored inside @MVar@
-- and @Nothing@ is stored on when connection is closed. This way user
-- won't be able to use now invalid connection,
newtype LevelDB = LevelDB (MVar (Maybe DB))

-- | Open handle to a database
openLevelDB :: MonadIO m => FilePath -> Options -> m LevelDB
openLevelDB path opt = liftIO $ do
  mv <- newEmptyMVar
  db <- open path opt
  putMVar mv (Just db)
  pure $ LevelDB mv

-- | Close and then immediately reopen database
moveLevelDbHandle :: MonadIO m => LevelDB -> LevelDB -> m ()
moveLevelDbHandle (LevelDB src) (LevelDB dst) = liftIO $ modifyMVar src $ \case
  Nothing -> error "moveLevelDbHandle: can't move from closed handle"
  Just h  -> modifyMVar dst $ \mh -> do
    mapM_ unsafeClose mh
    pure (Just h, (Nothing, ()))

-- | Close handle. After close it's no longer usable and any database
-- operation which uses this handle will throw exception.
closeLevelDB :: MonadIO m => LevelDB -> m ()
closeLevelDB (LevelDB mv) = liftIO $ do
  modifyMVar mv $ \mconn ->
    (Nothing, ()) <$ mapM_ unsafeClose mconn

usingLevelDB :: MonadIO m => LevelDB -> (DB -> IO a) -> m a
usingLevelDB (LevelDB h) action = liftIO $ withMVar h $ \case
  Nothing -> error "usingLevelDB: database is already closed"
  Just c  -> action c


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
