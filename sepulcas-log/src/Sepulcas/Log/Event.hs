module Sepulcas.Log.Event(
    logEvent
  , logEventWith
  ) where

import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Reflex
import Sepulcas.Native

logEvent :: (PerformEvent t m, PlatformNatives, Show a, MonadIO (Performable m)) => Text -> Event t a -> m (Event t a)
logEvent t e = performEvent $ ffor e $ \v -> do
  logWrite $ t <> (pack . show) v
  pure v

logEventWith :: (PerformEvent t m, PlatformNatives, MonadIO (Performable m)) => (a -> Text) -> Event t a -> m (Event t a)
logEventWith f e = performEvent $ ffor e $ \v -> do
  logWrite $ f v
  pure v
