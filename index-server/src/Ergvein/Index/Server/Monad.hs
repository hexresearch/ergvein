module Ergvein.Index.Server.Monad where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Servant.Server
import Servant.Server.Generic
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.DB.Monad
import Control.Monad.IO.Unlift
import Ergvein.Index.Server.Cache.Monad


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

instance MonadDB ServerM where
  getDbPool = asks envPersistencePool
  {-# INLINE getDbPool #-}

instance MonadLDB ServerM where
  getDb = asks envLevelDBContext
  {-# INLINE getDb #-}

instance MonadUnliftIO ServerM where
  askUnliftIO = ServerM $ (\(UnliftIO run) -> UnliftIO $ run . unServerM) <$> askUnliftIO
  withRunInIO go = ServerM $ withRunInIO (\k -> go $ k . unServerM)