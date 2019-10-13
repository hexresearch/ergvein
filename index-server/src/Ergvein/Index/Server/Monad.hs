module Ergvein.Index.Server.Monad where

import Control.Concurrent
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Data.ByteString.UTF8
import Servant.Server
import Servant.Server.Generic
import Database.Persist.Sql
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema


data ServerEnv = ServerEnv 
  { envConfig :: !Config
  , envLogger :: !(Chan (Loc, LogSource, LogLevel, LogStr))
  , envPool   :: !DBPool
  }

newServerEnv :: MonadIO m => Config -> m ServerEnv
newServerEnv cfg = do
  logger <- liftIO newChan
  pool <- liftIO . runStdoutLoggingT $ do
    pool <- newDBPool $ fromString $ configDb cfg
    flip runReaderT pool $ runDb $ runMigration migrateAll
    pure pool
  pure ServerEnv { envConfig = cfg
                 , envLogger = logger
                 , envPool   = pool
                 }

newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader ServerEnv, MonadThrow, MonadCatch, MonadMask)

type AsServerM = AsServerT ServerM

catchHandler :: IO a -> Handler a
catchHandler = Handler . ExceptT . try

runServerM :: ServerEnv -> ServerM a -> Handler a
runServerM e = catchHandler . runChanLoggingT (envLogger e) . flip runReaderT e . unServerM

runServerMIO :: ServerEnv -> ServerM a -> IO a
runServerMIO env m = do
  ea <- runHandler $ runServerM env m
  case ea of
    Left e -> fail $ "runServerMIO: " <> show e
    Right a -> return a