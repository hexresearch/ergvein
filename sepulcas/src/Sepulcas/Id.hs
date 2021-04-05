module Sepulcas.Id(
    genId
  ) where

import Control.Monad.IO.Class
import Data.IORef
import Data.Text (Text)
import Sepulcas.Text
import System.IO.Unsafe (unsafePerformIO)

-- | Generate new unique id
genId :: MonadIO m => m Text
genId = do
  i <- liftIO $ atomicModifyIORef' idRef $ \i -> (i+1, i)
  pure $ showt i

idRef :: IORef Int
idRef = unsafePerformIO $ newIORef 0
{-# NOINLINE idRef #-}
