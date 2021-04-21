module Ergvein.Core.Password(
    PasswordAsk(..)
  , requestPasssword
  ) where

import Reflex
import Data.Text (Text)
import Control.Monad.Random
import Control.Monad.IO.Class

class MonadIO m => PasswordAsk t m | m -> t where
  -- | Get event and trigger for pasword requesting modal. Int -- id of the request.
  getPasswordModalEF :: m (Event t (Int, Text), (Int, Text) -> IO ())
  -- | Get event and trigger for the event that the password was submitted from modal. Internal
  -- Nothing value means that the modal was dismissed
  getPasswordSetEF :: m (Event t (Int, Maybe Text), (Int, Maybe Text) -> IO ())

-- | Proper requester of passwords. Event contains login value
requestPasssword :: (PerformEvent t m, PasswordAsk t m, MonadIO (Performable m)) => Event t Text -> m (Event t Text)
requestPasssword reqE = do
  i <- liftIO getRandom
  modalF <- fmap snd getPasswordModalEF
  setE <- fmap fst getPasswordSetEF
  performEvent_ $ (liftIO . modalF . (i,)) <$> reqE
  pure $ fforMaybe setE $ \(i', mp) -> if i == i' then mp else Nothing
