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
  , writeLDB
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Database.LevelDB.Base
import Database.LevelDB.Internal (unsafeClose)

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

-- | Wrapped 'write'
writeLDB :: MonadIO m => LevelDB -> WriteOptions -> WriteBatch -> m ()
writeLDB db opt batch = usingLevelDB db $ \c -> write c opt batch
