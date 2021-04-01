module Ergvein.Wallet.Log.Event(
    logEvent
  , logEventWith
  ) where

import Data.Text (Text)
import Ergvein.Text
import Ergvein.Wallet.Monad.Prim
import Sepulcas.Native
import Reflex

logEvent :: (MonadBaseConstr t m, PlatformNatives, Show a) => Text -> Event t a -> m (Event t a)
logEvent t e = performEvent $ ffor e $ \v -> do
  logWrite $ t <> showt v
  pure v

logEventWith :: (MonadBaseConstr t m, PlatformNatives) => (a -> Text) -> Event t a -> m (Event t a)
logEventWith f e = performEvent $ ffor e $ \v -> do
  logWrite $ f v
  pure v