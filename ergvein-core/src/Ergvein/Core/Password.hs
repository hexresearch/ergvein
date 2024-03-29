module Ergvein.Core.Password(
    PasswordValidationResult(..)
  , PasswordAsk(..)
  , requestPasssword
  , PassEnv(..)
  , HasPassEnv(..)
  , newPassEnv
  , runPass
  ) where

import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Reader
import Data.Text (Text)
import Reflex
import Reflex.Flunky

data PasswordValidationResult = PasswordValid | PasswordInvalid deriving (Eq, Show)

class MonadIO m => PasswordAsk t m | m -> t where
  -- | Get event and trigger for pasword requesting modal. Int -- id of the request.
  -- Event t PasswordValidationResult -- an event that is triggered after password validation.
  getPasswordModalEF :: m (Event t (Int, Event t PasswordValidationResult, Text), (Int, Event t PasswordValidationResult, Text) -> IO ())
  -- | Get event and trigger for the event that the password was submitted from modal. Internal
  -- Nothing value means that the modal was dismissed
  getPasswordSetEF :: m (Event t (Int, Maybe Text), (Int, Maybe Text) -> IO ())

-- | Proper requester of passwords. Event contains login value
requestPasssword :: (PerformEvent t m, PasswordAsk t m, MonadIO (Performable m)) => Event t Text -> Event t PasswordValidationResult -> m (Event t Text)
requestPasssword reqE passValidationResultE = do
  i <- liftIO getRandom
  modalF <- fmap snd getPasswordModalEF
  setE <- fmap fst getPasswordSetEF
  performEvent_ $ liftIO . modalF . (i,passValidationResultE,) <$> reqE
  pure $ fforMaybe setE $ \(i', mp) -> if i == i' then mp else Nothing

data PassEnv t = PassEnv {
  penv'passModalEF :: !(EventTrigger t (Int, Event t PasswordValidationResult, Text))
, penv'passSetEF   :: !(EventTrigger t (Int, Maybe Text))
}

class Monad m => HasPassEnv t m | m -> t where
  getPassEnv :: m (PassEnv t)

instance {-# OVERLAPPABLE #-} Monad m => HasPassEnv t (ReaderT (PassEnv t) m) where
  getPassEnv = ask
  {-# INLINE getPassEnv #-}

instance {-# OVERLAPPABLE #-} (HasPassEnv t m, MonadIO m) => PasswordAsk t m where
  getPasswordModalEF = triggerPair . penv'passModalEF <$> getPassEnv
  {-# INLINE getPasswordModalEF #-}
  getPasswordSetEF = triggerPair . penv'passSetEF <$> getPassEnv
  {-# INLINE getPasswordSetEF #-}

newPassEnv :: (MonadIO m, TriggerEvent t m) => m (PassEnv t)
newPassEnv = PassEnv <$> newTriggerEvent' <*> newTriggerEvent'

runPass :: PassEnv t -> ReaderT (PassEnv t) m a -> m a
runPass = flip runReaderT
