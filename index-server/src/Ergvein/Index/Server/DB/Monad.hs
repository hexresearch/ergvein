module Ergvein.Index.Server.DB.Monad where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Pool
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.LevelDB

type DBPool = Pool SqlBackend

newDBPool :: (MonadUnliftIO m)
  => Bool -- ^ Use debug logger
  -> ConnectionString -> LoggingT m DBPool
newDBPool dbgLog connectionString = logger $ createPostgresqlPool connectionString 10
  where
    logger = if dbgLog then id
      else filterLogger (\_ lvl -> lvl /= LevelDebug)

class (MonadLogger m, MonadUnliftIO m) => MonadDB m where
    getDbPool :: m DBPool

type QueryT m a = ReaderT SqlBackend m a

instance (MonadLogger m, MonadUnliftIO m) => MonadDB (ReaderT DBPool m) where
    getDbPool = ask
    {-# INLINE getDbPool #-}

runDb :: MonadDB m => ReaderT SqlBackend m a -> m a
runDb ma = do
    pool <- getDbPool
    runSqlPool ma pool

runDbQuery :: MonadIO m => DBPool -> QueryT (ReaderT DBPool (LoggingT IO)) a -> m a
runDbQuery pool query = liftIO $ runStdoutLoggingT $ flip runReaderT pool $ runDb query
