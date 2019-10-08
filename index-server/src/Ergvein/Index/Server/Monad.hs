module Ergvein.Index.Service.Monad where

data ServerEnv = ServerEnv { }

newEnv :: MonadIO m => m ServerEnv
newEnv = pure ServerEnv {}

type AsServerM = AsServerT ServerM

newtype ServerM a = ServerM { unServerM :: ReaderT Env (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Env, MonadThrow, MonadCatch, MonadMask)
