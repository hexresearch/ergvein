module Ergvein.Index.Server.DB.Monad where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Pool
import Database.Persist.Sql
import Database.Persist.Postgresql

type DBPool = Pool SqlBackend

initConnectionPool ::(MonadUnliftIO m, MonadLogger m) => ConnectionString -> m DBPool
initConnectionPool connectionString = createPostgresqlPool connectionString 10

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